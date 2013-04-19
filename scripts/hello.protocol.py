from erlport import Port, Protocol, String


# Inherit custom protocol from erlport.Protocol
class HelloProtocol(Protocol):

    # Function handle_NAME will be called for incoming tuple {NAME, ...}
    def handle_hello(self, name):
        # String wrapper forces name to be a string instead of a list
        return "Hello, %s" % String(name)


if __name__ == "__main__":
    proto = HelloProtocol()
    # Run protocol with port open on STDIO
    proto.run(Port(use_stdio=True))
