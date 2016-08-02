Websocket Proof of Concept
==========================

A simple websocket server.

1. Accepts connections
2. Manages connections in a global TVar
3. Asynchronously pushes input from stdin to all connected clients
4. Asynchronously, and repeatedly pushes timed message to connected clients
