import { useState, useRef, useCallback, useEffect } from 'react';

import { useGameContext } from '../Game';

import Matter from 'matter-js';
import RunnerSprite from './sprite';

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
    Matter.Body.setInertia(bodyRef.current, Infinity);
    Matter.Composite.add(world, bodyRef.current);
    return () => {
      Matter.Composite.remove(world, bodyRef.current);
    };
  }, [world]);

  // Set up support for rendering the active player
  useEffect(() => renderer.addPass(renderActiveRunner), [renderer]);


  // Keyboard input processing
  const [leftPressed, setLeftPressed] = useState(false);
  const [rightPressed, setRightPressed] = useState(false);
  const [upPressed, setUpPressed] = useState(false);
  const keyDownHandler = useCallback((event) => {
    const { key } = event;
    if (key.startsWith('Arrow')) event.preventDefault();
    if (key === 'ArrowLeft') setLeftPressed(true);
    else if (key === 'ArrowRight') setRightPressed(true);
    else if (key === 'ArrowUp') setUpPressed(true);
  }, []);
  const keyUpHandler = useCallback((event) => {
    const { key } = event;
    if (key === 'ArrowLeft') setLeftPressed(false);
    else if (key === 'ArrowRight') setRightPressed(false);
    else if (key === 'ArrowUp') setUpPressed(false);
  }, []);
  useEffect(() => {
    window.addEventListener('keydown', keyDownHandler);
    window.addEventListener('keyup', keyUpHandler);
    return () => {
      window.removeEventListener('keydown', keyDownHandler);
      window.removeEventListener('keyup', keyUpHandler);
    };
  }, [keyDownHandler, keyUpHandler]);

  // Left/right movement
  const keepGoingRight = useCallback(() => {
    Matter.Body.setVelocity(bodyRef.current, { x: 2, y: bodyRef.current.velocity.y });
  }, []);
  const keepGoingLeft = useCallback(() => {
    Matter.Body.setVelocity(bodyRef.current, { x: -2, y: bodyRef.current.velocity.y });
  }, []);
  useEffect(() => {
    if (rightPressed && !leftPressed) {
      events.off('beforeFrame', keepGoingLeft);
      events.on('beforeFrame', keepGoingRight);
      spriteRef.current.setDirection(1);
    } else if (leftPressed && !rightPressed) {
      events.off('beforeFrame', keepGoingRight);
      events.on('beforeFrame', keepGoingLeft);
      spriteRef.current.setDirection(-1);
    } else {
      events.off('beforeFrame', keepGoingRight);
      events.off('beforeFrame', keepGoingLeft);
    }
    return () => {
      events.off('beforeFrame', keepGoingRight);
      events.off('beforeFrame', keepGoingLeft);
    };
  }, [rightPressed, leftPressed, keepGoingRight, keepGoingLeft, events]);

  // Jumping
  useEffect(() => {
    const { x: currentXVelocity } = bodyRef.current.velocity;

    if (upPressed) {
      // Check that the player is standing on a platform
      const runnerBounds = Matter.Bounds.create(bodyRef.current.vertices);
      const platformDetectionBounds = {
        min: { x: runnerBounds.min.x, y: runnerBounds.max.y },
        max: { x: runnerBounds.max.x, y: runnerBounds.max.y + 1 },
      };
      const bodies = Matter.Composite.allBodies(world);
      const standingOn = Matter.Query.region(bodies, platformDetectionBounds)
        .filter((body) => ['ground', 'platform'].includes(body.label));
      // Jumping is allowed if we're standing on a platform
      if (standingOn.length > 0) {
        Matter.Body.setVelocity(bodyRef.current, { x: currentXVelocity, y: -5 });
      }
    }
  }, [upPressed, bodyRef, world]);

  return null;
}
