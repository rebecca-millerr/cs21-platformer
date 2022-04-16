import React, { useMemo, useCallback, useRef, useEffect } from 'react';
import { BLOCKS_ACROSS, BLOCKS_DOWN, BLOCK_SIZE, DEFAULT_BLOCK as DEFAULT_BLOCK_COLOR, MOVING_SPEED } from './constants';
import Matter from 'matter-js';

import classNames from 'classnames/bind';
import styles from './map.module.css';
const cx = classNames.bind(styles);

/* Render the contents of a Matter.js world to a canvas */
const drawScene = (context, bodies, offset) => {
  context.clearRect(0, 0, context.canvas.width, context.canvas.height);

  bodies.forEach((body) => {
    context.beginPath();
    body.vertices.forEach(({ x, y }) => context.lineTo(x - offset, y));
    context.closePath();
    context.fillStyle = body.render.fillStyle ?? '#000';
    context.fill();
  });
};

export default function Map() {
  const canvasRef = useRef();
  const canvasContextRef = useRef();

  const engine = useMemo(() => Matter.Engine.create(), []);
  const world = useMemo(() => engine.world, [engine]);
  const xOffsetRef = useRef(0);

  /* Maintenance surrounding moving the viewport: keeps the ground underfoot, and "garbage collects"
     blocks that have exited the left side of the screen */
  const viewportMoved = useCallback(() => {
    Matter.Composite.allBodies(world).forEach((body) => {
      const { vertices } = body;
      const leftEdge = xOffsetRef.current;
      // Move ground
      if (body.label === 'ground') {
        const width = body.bounds.max.x - body.bounds.min.x;
        Matter.Body.setPosition(body, { x: leftEdge + width / 2, y: body.position.y });
      // Garbage collection
      // Remove objects once they are a full BLOCK_SIZE off screen - not immediately
      } else if (vertices.every(({ x }) => x < leftEdge - BLOCK_SIZE)) {
        console.log('removing body that went off screen');
        Matter.Composite.remove(world, body);
      }
    });
  }, [world]);

  // Render loop
  const requestRef = useRef();
  const previousTimeRef = useRef();
  const animate = useCallback((time) => {
    // Timekeeping
    const delta = (time && previousTimeRef.current)
      ? (time - previousTimeRef.current) / 1000 // seconds
      : 0; // no delta if no previous time (i.e. on first render)
    previousTimeRef.current = time;
    // Move viewport
    xOffsetRef.current += delta * MOVING_SPEED;
    viewportMoved();
    // Run physics simulation
    Matter.Engine.update(engine, delta * 1000);
    // Get current state of physics simulation
    const bodies = Matter.Composite.allBodies(world);
    // Paint the picture
    drawScene(canvasContextRef.current, bodies, xOffsetRef.current);
    // On to the next frame
    requestRef.current = requestAnimationFrame(animate);
  }, [engine, world, viewportMoved]);

  useEffect(() => {
    // Create the ground
    const ground = Matter.Bodies.rectangle(
      // x, y specify the *center* of the object
      (BLOCKS_ACROSS * BLOCK_SIZE) / 2,
      (BLOCKS_DOWN - 0.5) * BLOCK_SIZE,
      BLOCKS_ACROSS * BLOCK_SIZE,
      BLOCK_SIZE,
      { isStatic: true, label: 'ground', render: { fillStyle: 'green' } },
    );
    Matter.Composite.add(world, ground);

    Matter.Composite.add(
      world,
      Matter.Bodies.rectangle(
        500, 100, BLOCK_SIZE, BLOCK_SIZE,
        { render: { fillStyle: DEFAULT_BLOCK_COLOR } },
      ),
    );
    // Get canvas context
    canvasContextRef.current = canvasRef.current.getContext('2d');
    // Start the render loop
    requestRef.current = requestAnimationFrame(animate);
    // Clean up
    return () => {
      Matter.Composite.remove(world, ground);
      cancelAnimationFrame(requestRef.current);
    };
  }, [world, animate]);

  return (
    <canvas
      ref={canvasRef}
      width={BLOCKS_ACROSS * BLOCK_SIZE}
      height={BLOCKS_DOWN * BLOCK_SIZE}
      className={cx('mapCanvas')}
    />
  );
}
