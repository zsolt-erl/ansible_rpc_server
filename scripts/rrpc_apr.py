#!/usr/bin/python
# (C) 2012, Michael DeHaan, <michael.dehaan@gmail.com>

# This file is part of Ansible
#
# Ansible is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Ansible is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Ansible.  If not, see <http://www.gnu.org/licenses/>.

#######################################################
# 21 Mar 2013 - Zsolt Keszthelyi
#    modified to work with rabbitrpc services
#    only modification is to return playbook_out registered variable on stdout
#    (would be nice to be able to quiet down the Playbook object completely (verbose=shut_up), 
#    that way the only output would be the playbook_out JSON object )


import sys
import getpass
import os
import json

import ansible.playbook
import ansible.constants as C
from ansible import errors
from ansible import callbacks
from ansible import utils
from ansible.color import ANSIBLE_COLOR, stringc

def colorize(lead, num, color):
    """ Print 'lead' = 'num' in 'color' """
    if num != 0 and ANSIBLE_COLOR:
        return "%s%s%-15s" % (stringc(lead, color), stringc("=", color), stringc(str(num), color))
    else:
        return "%s=%-4s" % (lead, str(num))

def hostcolor(host, stats):
    if ANSIBLE_COLOR:
        if stats['failures'] != 0 or stats['unreachable'] != 0:
            return "%-41s" % stringc(host, 'red')
        elif stats['changed'] != 0:
            return "%-41s" % stringc(host, 'yellow')
        else:
            return "%-41s" % stringc(host, 'green')
    return "%-30s" % host


def main(args):
    ''' run ansible-playbook operations '''

    # create parser for CLI options
    usage = "%prog playbook.yml"
    parser = utils.base_parser(
        constants=C, 
        usage=usage, 
        connect_opts=True, 
        runas_opts=True, 
        subset_opts=True, 
        check_opts=True,
        diff_opts=True
    )
    parser.add_option('-e', '--extra-vars', dest="extra_vars", default=None,
        help="set additional key=value variables from the CLI")
    parser.add_option('-t', '--tags', dest='tags', default='all',
        help="only run plays and tasks tagged with these values")
    # FIXME: list hosts is a common option and can be moved to utils/__init__.py
    parser.add_option('--list-hosts', dest='listhosts', action='store_true',
        help="dump out a list of hosts, each play will run against, does not run playbook!")
    parser.add_option('--syntax-check', dest='syntax', action='store_true',
        help="do a playbook syntax check on the playbook, do not execute the playbook")
    parser.add_option('--list-tasks', dest='listtasks', action='store_true',
        help="do list all tasks that would be executed")

    options, args = parser.parse_args(args)

    if len(args) == 0:
        parser.print_help(file=sys.stderr)
        return 1

    inventory = ansible.inventory.Inventory(options.inventory)
    inventory.subset(options.subset)
    if len(inventory.list_hosts()) == 0:
        raise errors.AnsibleError("provided hosts list is empty")

    sshpass = None
    sudopass = None
    if not options.listhosts and not options.syntax and not options.listtasks:
        options.ask_pass = options.ask_pass or C.DEFAULT_ASK_PASS
        if options.ask_pass:
            sshpass = getpass.getpass(prompt="SSH password: ")
        options.ask_sudo_pass = options.ask_sudo_pass or C.DEFAULT_ASK_SUDO_PASS
        if options.ask_sudo_pass:
            sudopass = getpass.getpass(prompt="sudo password: ")
            options.sudo = True
        if options.sudo_user:
            options.sudo = True
        options.sudo_user = options.sudo_user or C.DEFAULT_SUDO_USER
    extra_vars = utils.parse_kv(options.extra_vars)
    only_tags = options.tags.split(",")

    for playbook in args:
        if not os.path.exists(playbook):
            raise errors.AnsibleError("the playbook: %s could not be found" % playbook)
        if not os.path.isfile(playbook):
            raise errors.AnsibleError("the playbook: %s does not appear to be a file" % playbook)

    # run all playbooks specified on the command line
    for playbook in args:

        
        (playbook_path, playbook_file) = os.path.split(playbook)
        os.chdir(playbook_path)


        stats = callbacks.AggregateStats()
        playbook_cb = callbacks.PlaybookCallbacks(verbose=utils.VERBOSITY)
        runner_cb = callbacks.PlaybookRunnerCallbacks(stats, verbose=utils.VERBOSITY)

        pb = ansible.playbook.PlayBook(
            playbook=playbook_file,
            module_path=options.module_path,
            inventory=inventory,
            forks=options.forks,
            remote_user=options.remote_user,
            remote_pass=sshpass,
            callbacks=playbook_cb,
            runner_callbacks=runner_cb,
            stats=stats,
            timeout=options.timeout,
            transport=options.connection,
            sudo=options.sudo,
            sudo_user=options.sudo_user,
            sudo_pass=sudopass,
            extra_vars=extra_vars,
            private_key_file=options.private_key_file,
            only_tags=only_tags,
            check=options.check,
            diff=options.diff
        )

        if options.listhosts or options.listtasks:
            print ''
            print 'playbook: %s' % playbook
            print ''
            playnum = 0
            for (play_ds, play_basedir) in zip(pb.playbook, pb.play_basedirs):
                playnum += 1
                play = ansible.playbook.Play(pb, play_ds, play_basedir)
                label = play.name
                if options.listhosts:
                    hosts = pb.inventory.list_hosts(play.hosts)
                    print '  play #%d (%s): host count=%d' % (playnum, label, len(hosts))
                    for host in hosts:
                        print '    %s' % host
                if options.listtasks:
                    matched_tags, unmatched_tags = play.compare_tags(pb.only_tags)
                    unmatched_tags.discard('all')
                    unknown_tags = set(pb.only_tags) - (matched_tags | unmatched_tags)
                    if unknown_tags:
                        msg = 'tag(s) not found in playbook: %s.  possible values: %s'
                        unknown = ','.join(sorted(unknown_tags))
                        unmatched = ','.join(sorted(unmatched_tags))
                        raise errors.AnsibleError(msg % (unknown, unmatched))
                    print '  play #%d (%s): task count=%d' % (playnum, label, len(play.tasks()))
                    for task in play.tasks():
                        if set(task.tags).intersection(pb.only_tags):
                            print '    %s' % task.name
                print ''
            continue

        if options.syntax:
            # if we've not exited by now then we are fine.
            print 'Playbook Syntax is fine'
            return 0
            

        try:

            pb.run()

            hosts = sorted(pb.stats.processed.keys())
            print callbacks.banner("PLAY RECAP")
            playbook_cb.on_stats(pb.stats)
            for h in hosts:
                t = pb.stats.summarize(h)
                print "%-30s : %s %s %s %s " % (
                    hostcolor(h, t),
                    colorize('ok', t['ok'], 'green'),
                    colorize('changed', t['changed'], 'yellow'),
                    colorize('unreachable', t['unreachable'], 'red'),
                    colorize('failed', t['failures'], 'red'))

            print "\n"
            for h in hosts:
                stats = pb.stats.summarize(h)
                if stats['failures'] != 0 or stats['unreachable'] != 0:
                    return 2

            print "\n"
            print "PLAYBOOK_OUT"
            print "\n"
            playbook_out={}

            for h in hosts:
                if "playbook_out" in pb.SETUP_CACHE[h]: 
                    playbook_out[h] = pb.SETUP_CACHE[h]["playbook_out"]

            print json.dumps( playbook_out )

        except errors.AnsibleError, e:
            print >>sys.stderr, "ERROR: %s" % e
            return 1

    return 0






if __name__ == "__main__":
    try:
        sys.exit(main(sys.argv[1:]))
    except errors.AnsibleError, e:
        print >>sys.stderr, "ERROR: %s" % e
        sys.exit(1)

