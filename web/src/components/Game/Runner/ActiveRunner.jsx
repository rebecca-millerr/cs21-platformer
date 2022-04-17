import { useEffect, useRef } from 'react';

import { useGameContext } from '../Game';

import Matter from 'matter-js';
import RunnerSprite from './sprite';

function renderActiveRunner(gameContext) {
  Matter.Composite.allBodies(gameContext.world).forEach((body) => {
    if (body.label !== 'active-runner') return;

    const { x, y } = body.position;
    const { sprite } = body.render;
    const renderedRunner = sprite.getCanvas();
    const { width, height } = renderedRunner;
    const xOffset = gameContext.xOffsetRef.current;
    const canvasContext = gameContext.canvasContextRef.current;
    canvasContext.drawImage(renderedRunner, x - (width / 2) - xOffset, y - (height / 2));
    sprite.tick();
  });
}

/* Runner that is controlled by this player */
export default function ActiveRunner() {
  const { renderer, world } = useGameContext();

  // Create the active runner and add it to the world
  const spriteRef = useRef();
  const bodyRef = useRef();
  useEffect(() => {
    spriteRef.current = new RunnerSprite();
    bodyRef.current = Matter.Bodies.rectangle(
      400, 100, spriteRef.current.width - 25, spriteRef.current.height,
      {
        label: 'active-runner',
        render: { sprite: spriteRef.current },
      },
    );
    Matter.Composite.add(world, bodyRef.current);
    return () => {
      Matter.Composite.remove(world, bodyRef.current);
    };
  }, [world]);

  // Set up support for rendering the active player
  useEffect(() => renderer.addPass(renderActiveRunner), [renderer]);

  return null;
}
