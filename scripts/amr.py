#!/usr/bin/python

import ansible.runner
import sys
import json

# import ansible.inventory

Pattern    = sys.argv[1]
ModuleName = sys.argv[2]
ModuleArgs = sys.argv[3]


# construct the ansible runner and execute on all hosts
runner = ansible.runner.Runner(
        pattern = Pattern, 
        module_name = ModuleName, 
        module_args = ModuleArgs,
        sudo = True,
        forks = 10
)

results = runner.run()

if results is None:
   print "{}"
   sys.exit(1)
else:
    print json.dumps(results)

