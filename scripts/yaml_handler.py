from erlport import Port, Protocol
from erlport.erlterms import Atom, encode, decode, String
import yaml
import sys
import datetime

class YAMLHandler(Protocol):

    #def __init__(self):
    #    self.collected = ["hello"]

    def handle(self, port, message):
        log=open("/tmp/rrpcpy.log", 'a')
        print >>log, message
        log.close()

        if message[0] == Atom("load_yaml"):
            filename = message[1]   # filename has to be a binary() on erlang side (python strings are represented with binaries)
            f = open(filename, 'r')
            yaml_data = yaml.load(f)
            f.close()
            port.write( yaml_data ) 

        elif message[0] == Atom("dump_yaml"):
            filename = message[1]   # filename has to be a binary() on erlang side (python strings are represented with binaries)

            yaml_doc = self.convert_term( message[2] )

            scriptname = sys.argv[0]
            formatted_datetime = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")

            f = open(filename, 'w')
            # print >>f, "---"
            print >>f, "## created by %s on %s\n" % (scriptname, formatted_datetime)
            print >>f, yaml.dump(yaml_doc, default_flow_style=False, explicit_start=True)
            f.close()

            port.write( "done" ) 

    def isproplist(self, x):
        if isinstance(x, list):
            ## scan the whole list
            for elem in x:
                try:
                    (key, value) = elem
                except:
                    return False
            return True
        else:
            return False


    ## converts a term that comes from erlang to the python equvivalent (tries to find all proplists in the term and convert them to dict objects)
    def convert_term(self, term):
        if self.isproplist(term):
            newdict = {}
            for (key, value) in term:
                newdict[key] = self.convert_term(value)
            return newdict
        elif isinstance(term, list):
            newlist = []
            newlist = [ self.convert_term(elem) for elem in term ]
            return newlist
        else:
            return term

if __name__ == "__main__":
    proto = YAMLHandler()
    proto.run(Port(packet=4, use_stdio=True))
