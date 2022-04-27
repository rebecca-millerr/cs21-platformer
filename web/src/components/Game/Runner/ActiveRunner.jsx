import Matter from 'matter-js';
import Runner from './controlled-runner';

import { useRef, useEffect, useMemo } from 'react';
import { useGameContext } from '../Game';
import { throttle } from 'lodash';

import { BLOCK_SIZE } from '../constants';


function renderActiveRunner(gameContext) {
  const canvasContext = gameContext.canvasContextRef.current;
  const xOffset = gameContext.xOffsetRef.current;

  Matter.Composite.allBodies(gameContext.world).forEach((body) => {
    if (body.label !== 'active-runner') return;

    // Draw sprite
    const { x, y } = body.position;
    const { sprite } = body.render;
    const renderedRunner = sprite.getCanvas();
    const { width, height } = renderedRunner;
    canvasContext.drawImage(renderedRunner, x - (width / 2) - xOffset, y - (height / 2));
    sprite.tick();
  });
}

/* Runner that is controlled by this player */
export default function ActiveRunner() {
  const { renderer, world, events, socket, xOffsetRef } = useGameContext();

  // Create the active runner and add it to the world
  const runnerRef = useRef();
  useEffect(() => {
    const createRunner = () => {
      runnerRef.current = new Runner(world, xOffsetRef.current + (BLOCK_SIZE * 2));
      events.off('positionFound', createRunner);
    };
    events.on('positionFound', createRunner);

    return () => {
      events.off('positionFound', createRunner);
      if (runnerRef.current) runnerRef.current.remove();
    };
  }, [world, events, xOffsetRef]);

  // Set up support for rendering the active player
  useEffect(() => renderer.addPass(renderActiveRunner), [renderer]);

  // Set up key event handling
  useEffect(() => {
    const onKeyDown = (e) => runnerRef.current?.onKeyDown?.(e);
    const onKeyUp = (e) => runnerRef.current?.onKeyUp?.(e);
    window.addEventListener('keydown', onKeyDown);
    window.addEventListener('keyup', onKeyUp);
    return () => {
      window.removeEventListener('keydown', onKeyDown);
      window.removeEventListener('keyup', onKeyUp);
    };
  });

  const sendServerUpdate = useMemo(() => throttle(() => {
    if (!runnerRef.current) return;
    const { x, y } = runnerRef.current.body.position;
    socket.cast('update', { pos: { x, y } }).catch(() => {});
  }, 50), [socket]);

  // Let runner perform necessary updates to itself on every frame
  useEffect(() => {
    const update = () => {
      runnerRef.current?.update?.();
      sendServerUpdate();
    };
    events.on('beforeFrame', update);
    return () => events.off('beforeFrame', update);
  }, [events, runnerRef, sendServerUpdate]);

  return null;
}
