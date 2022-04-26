import React, { useRef, useEffect } from 'react';
import Matter from 'matter-js';
import { MOVING_SPEED } from './constants';

import { useGameContext } from './Game';

export default function DebugPane() {
  const debugPaneRef = useRef();
  const gameContext = useGameContext();

  const tickXRef = useRef(null);
  useEffect(() => {
    const update = ({ ticks }) => {
      if (ticks) tickXRef.current = (ticks / 8) * MOVING_SPEED;
    };
    gameContext.events.on('socket_message', update);
    return () => gameContext.events.off('socket_message', update);
  }, [gameContext.events]);

  useEffect(() => {
    const lastUpdateTimes = new Array(5).fill(0);
    const pairIndices = lastUpdateTimes.map((_, i) => i).slice(0, -1);
    const update = () => {
      // FPS
      const now = performance.now();
      pairIndices.forEach((i) => { lastUpdateTimes[i] = lastUpdateTimes[i + 1]; });
      lastUpdateTimes[lastUpdateTimes.length - 1] = now;
      const fps = (a, b) => 1000 / (b - a);
      const pairwiseFps = pairIndices.map((i) => fps(lastUpdateTimes[i], lastUpdateTimes[i + 1]));
      const averageFps = pairwiseFps.reduce((a, b) => a + b, 0) / pairwiseFps.length;

      debugPaneRef.current.innerHTML = `
Horizontal scroll:     ${gameContext.xOffsetRef.current?.toFixed?.(3)}
  Target from server:  ${tickXRef.current?.toFixed?.(3)}
Player type:           ${gameContext.playerType}
Bodies in world:       ${Matter.Composite.allBodies(gameContext.world).length}
Socket connected?      ${gameContext.socket.connected}
  Own ID:              ${gameContext.ownId}
Render FPS:            ${averageFps.toFixed(1)}
      `;
    };
    gameContext.events.on('afterFrame', update);
    return () => gameContext.events.off('afterFrame', update);
  }, [gameContext]);

  return (
    <div
      ref={debugPaneRef}
      style={{
        position: 'absolute',
        top: 'calc(100% - 0.5em)',
        left: '0px',
        whiteSpace: 'pre',
        fontFamily: 'monospace',
      }}
    />
  );
}
