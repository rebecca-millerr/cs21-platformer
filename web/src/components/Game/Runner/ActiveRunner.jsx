import { useRef, useEffect } from 'react';

import { useGameContext } from '../Game';

import Matter from 'matter-js';
import Runner from './runner';

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
    // Debug: draw vertices of underlying body
    canvasContext.beginPath();
    body.vertices.forEach((vert) => canvasContext.lineTo(vert.x - xOffset, vert.y));
    canvasContext.closePath();
    canvasContext.strokeStyle = '#000';
    canvasContext.stroke();
  });
}

/* Runner that is controlled by this player */
export default function ActiveRunner() {
  const { renderer, world, events } = useGameContext();

  // Create the active runner and add it to the world
  const runnerRef = useRef();
  useEffect(() => {
    runnerRef.current = new Runner(world);
    return () => runnerRef.current.remove();
  }, [world]);

  // Set up support for rendering the active player
  useEffect(() => renderer.addPass(renderActiveRunner), [renderer]);

  // Set up key event handling
  useEffect(() => {
    const onKeyDown = (e) => runnerRef.current.onKeyDown(e);
    const onKeyUp = (e) => runnerRef.current.onKeyUp(e);
    window.addEventListener('keydown', onKeyDown);
    window.addEventListener('keyup', onKeyUp);
    return () => {
      window.removeEventListener('keydown', onKeyDown);
      window.removeEventListener('keyup', onKeyUp);
    };
  });

  // Let runner perform necessary updates to itself on every frame
  useEffect(() => {
    const update = () => runnerRef.current.update();
    events.on('beforeFrame', update);
    return () => events.off('beforeFrame', update);
  }, [events, runnerRef]);

  return null;
}
