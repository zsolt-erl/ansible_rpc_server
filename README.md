Ansible RPC Server
==================

JSON-RPC server with an Ansible Runner service. Accepts JSON-RPC 1.1 requests over RabbitMq .
( not production ready )


Needs a 'test' exchange declared.
Requests need to be published to 'rpc\_queue/service\_name' queue.

Based on: https://github.com/tonyg/erlang-rfc4627

For request format see:  rrpc\_test\_publisher:sample\_\request/0

Playbooks can register a playbook\_out variable that will be returned as a response to the RPC request.
