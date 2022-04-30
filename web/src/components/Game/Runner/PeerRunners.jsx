import StreamingInterpolator from 'helpers/streaming-interpolation';
import { useRef, useEffect, useCallback } from 'react';
import { useGameContext } from '../Game';


export default function PeerRunners() {
  const peerRunners = useRef({});

  // Track runner positions from server
  const { events, ownId, playerType } = useGameContext();
  useEffect(() => {
    const handleMessage = ({ runners }) => {
      if (!runners) return;
      const trackedIds = Object.keys(peerRunners.current);
      const currentIds = Object.keys(runners)
        .filter((id) => playerType === 'builder' || id !== ownId?.toString?.());

      const lostIds = trackedIds.filter((id) => !currentIds.includes(id));
      const newIds = currentIds.filter((id) => !trackedIds.includes(id));

      lostIds.forEach((id) => { delete peerRunners.current[id]; });
      newIds.forEach((id) => {
        peerRunners.current[id] = {
          x: new StreamingInterpolator(100),
          y: new StreamingInterpolator(100),
        };
      });

      currentIds.forEach((id) => {
        peerRunners.current[id].x.addPoint([runners[id].x, 0]);
        peerRunners.current[id].y.addPoint([runners[id].y, 0]);
      });
    };

    events.on('socket_message', handleMessage);
    return () => events.off('socket_message', handleMessage);
  }, [events, ownId, playerType]);


  // Rendering

  const { renderer, canvasContextRef, xOffsetRef } = useGameContext();
  const renderPeerRunners = useCallback(() => {
    const canvasContext = canvasContextRef.current;
    const xOffset = xOffsetRef.current;

    Object.values(peerRunners.current).forEach((runner) => {
      // TODO: render real sprite
      const x = runner.x.getCurrentValueWithFallback()?.x;
      const y = runner.y.getCurrentValueWithFallback()?.x;
      if (!x || !y) return;
      const width = 16.5;
      const height = 37;
      canvasContext.fillStyle = 'rgba(0, 0, 0, 0.25)';
      canvasContext
        .fillRect(x - (width / 2) - xOffset, y - (height / 2), width, height);
    });
  }, [canvasContextRef, xOffsetRef]);
  useEffect(
    () => renderer.addPass(renderPeerRunners),
    [renderer, renderPeerRunners],
  );

  return null;
}
