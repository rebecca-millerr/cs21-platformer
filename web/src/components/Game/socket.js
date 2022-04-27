import { useState, useEffect, useMemo, useCallback } from 'react';
import { WEBSOCKET_URL } from './constants';

export default function useSocketConnection(playerType, eventBus) {
  // Open socket connection and set up basic event listeners

  const [socket, setSocket] = useState(false); // raw WebSocket object
  const [connected, setConnected] = useState(false); // socket connection is open

  useEffect(() => {
    const sock = new WebSocket(WEBSOCKET_URL);
    setSocket(sock);

    sock.onopen = () => setConnected(true);
    sock.onmessage = (event) => {
      let { data } = event;
      try { data = JSON.parse(data); } catch (e) {}
      eventBus.emit('socket_message', data);
    };
    sock.onclose = () => setConnected(false);

    const heartbeat = setInterval(() => {
      if (sock.readyState === 1) sock.send('💓');
    }, 5000);

    return () => {
      sock.close();
      clearInterval(heartbeat);
    };
  }, [eventBus]);


  // Message sending

  // Make a cast (doesn't expect a response; resolves immediately)
  const cast = useCallback((type, data) => new Promise((resolve, reject) => {
    if (!socket || !connected) reject();
    socket.send(JSON.stringify({ cast: true, type, ...data }));
    resolve();
  }), [socket, connected]);

  // Make a call and wait for a response before resolving
  // recognizeRseponse should return true if a message looks like a response to the call that is
  // being made
  const call = useCallback((type, data, recognizeResponse) => new Promise((resolve, reject) => {
    if (!socket || !connected) reject();
    // Make the call
    socket.send(JSON.stringify({ call: true, type, ...data }));
    // Listen to all messages until we find one that looks like what we expect
    const listener = (event) => {
      if (recognizeResponse(event)) { // does this new message look like what we're waiting for?
        eventBus.off('socket_message', listener);
        resolve(event);
      }
    };
    eventBus.on('socket_message', listener);
  }), [socket, connected, eventBus]);


  // Join the game when the socket connection opens

  const [ownId, setOwnId] = useState(null);

  useEffect(() => {
    if (!socket || !connected) return;
    call(
      `become-${playerType}`,
      null,
      // expect response to "join" call to include our ID
      (obj) => Object.keys(obj).includes('id'),
    ).then(({ id }) => setOwnId(id));
  }, [socket, connected, playerType, call]);


  // External API

  return useMemo(() => ({
    _socket: socket, // low-level API
    socket: { connected, cast, call }, // high-level API
    ownId,
    joined: ownId !== null,
  }), [socket, connected, ownId, cast, call]);
}
