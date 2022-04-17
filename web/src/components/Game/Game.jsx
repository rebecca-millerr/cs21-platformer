import React, { useMemo, useCallback, useRef, useEffect } from 'react';
import PropTypes from 'prop-types';

import { BLOCKS_ACROSS, BLOCKS_DOWN, BLOCK_SIZE, COLORS, MOVING_SPEED } from './constants';
import Matter from 'matter-js';

import classNames from 'classnames/bind';
import styles from './Game.module.scss';
const cx = classNames.bind(styles);

/* Render the contents of a Matter.js world to a canvas */
const drawScene = (context, bodies, offset) => {
  context.clearRect(0, 0, context.canvas.width, context.canvas.height);

  // Draw bodies
  bodies.forEach((body) => {
    context.beginPath();
    body.vertices.forEach(({ x, y }) => context.lineTo(x - offset, y));
    context.closePath();
    context.fillStyle = body.render.fillStyle ?? '#000';
    context.fill();
  });

  // Draw grid lines
  // Latitude lines
  context.strokeStyle = 'rgba(0, 0, 0, 0.25)';
  for (let y = 0; y <= BLOCKS_DOWN; y += 1) {
    context.beginPath();
    context.moveTo(0, y * BLOCK_SIZE);
    context.lineTo(BLOCKS_ACROSS * BLOCK_SIZE, y * BLOCK_SIZE);
    context.stroke();
  }
  // Longitude lines
  const firstLongitudeLine = Math.floor(offset / BLOCK_SIZE);
  const lastLongitudeLine = firstLongitudeLine + BLOCKS_ACROSS + 1;
  for (let x = firstLongitudeLine; x <= lastLongitudeLine; x += 1) {
    context.beginPath();
    context.moveTo(x * BLOCK_SIZE - offset, 0);
    context.lineTo(x * BLOCK_SIZE - offset, BLOCKS_DOWN * BLOCK_SIZE);
    context.stroke();
  }
};


export default function Map({ allowBuilding }) {
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

  // Initial setup\
  useEffect(() => {
    // Create the ground
    const ground = Matter.Bodies.rectangle(
      // x, y specify the *center* of the object
      (BLOCKS_ACROSS * BLOCK_SIZE) / 2,
      (BLOCKS_DOWN - 0.5) * BLOCK_SIZE,
      BLOCKS_ACROSS * BLOCK_SIZE,
      BLOCK_SIZE,
      { isStatic: true, label: 'ground', render: { fillStyle: COLORS.GROUND } },
    );
    Matter.Composite.add(world, ground);

    Matter.Composite.add(
      world,
      Matter.Bodies.rectangle(
        BLOCK_SIZE * (BLOCKS_ACROSS - 2.5), BLOCK_SIZE * 1.5, BLOCK_SIZE, BLOCK_SIZE,
        { render: { fillStyle: COLORS.DEFAULT_BLOCK } },
      ),
    );
    // Set up canvas / context
    const dpr = window.devicePixelRatio || 1;
    canvasRef.current.width = BLOCKS_ACROSS * BLOCK_SIZE * dpr;
    canvasRef.current.height = BLOCKS_DOWN * BLOCK_SIZE * dpr;
    canvasContextRef.current = canvasRef.current.getContext('2d');
    canvasContextRef.current.setTransform(dpr, 0, 0, dpr, 0, 0);
    canvasContextRef.current.imageSmoothingEnabled = false;
    // Start the render loop
    requestRef.current = requestAnimationFrame(animate);
    // Clean up
    return () => {
      Matter.Composite.remove(world, ground);
      cancelAnimationFrame(requestRef.current);
    };
  }, [world, animate]);

  // Builders can place blocks
  const createBlock = (event) => {
    // Enforce that the user is allowed to build
    if (!allowBuilding) return;
    // Find out where in the world they clicked
    const { top, left } = canvasRef.current.getBoundingClientRect();
    const x = event.clientX - left + xOffsetRef.current;
    const y = event.clientY - top;
    // Create a block for that space
    const row = Math.floor(y / BLOCK_SIZE);
    const col = Math.floor(x / BLOCK_SIZE);
    const newBlock = Matter.Bodies.rectangle(
      (col + 0.5) * BLOCK_SIZE,
      (row + 0.5) * BLOCK_SIZE,
      BLOCK_SIZE,
      BLOCK_SIZE,
      { isStatic: true, render: { fillStyle: '#c934eb' } },
    );
    // Check that it doesn't overlap anything in the world (runners, the ground, other blocks)
    const collisions = Matter.Query.collides(newBlock, Matter.Composite.allBodies(world));
    if (collisions.length > 0) return;
    // If it doesn't, add it to the world
    Matter.Composite.add(world, newBlock);
  };

  return (
    <canvas
      ref={canvasRef}
      className={cx('canvas')}
      width={BLOCKS_ACROSS * BLOCK_SIZE}
      height={BLOCKS_DOWN * BLOCK_SIZE}
      style={{ width: `${BLOCKS_ACROSS * BLOCK_SIZE}px`, height: `${BLOCKS_DOWN * BLOCK_SIZE}px` }}
      onMouseDown={createBlock}
    />
  );
}

Map.propTypes = {
  allowBuilding: PropTypes.bool,
};
Map.defaultProps = {
  allowBuilding: false,
};
