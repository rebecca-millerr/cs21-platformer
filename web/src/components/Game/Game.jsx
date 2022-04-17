import React, { useMemo, useCallback, useRef, useEffect, createContext, useContext } from 'react';
import PropTypes from 'prop-types';

import { BLOCKS_ACROSS, BLOCKS_DOWN, BLOCK_SIZE, COLORS, MOVING_SPEED } from './constants';
import Matter from 'matter-js';
import drawScene from './renderer';

import classNames from 'classnames/bind';
import styles from './Game.module.scss';
const cx = classNames.bind(styles);

export const GameContext = createContext();
export const useGameContext = () => useContext(GameContext);


export default function Game({ children }) {
  const canvasRef = useRef();
  const canvasContextRef = useRef();

  const engine = useMemo(() => Matter.Engine.create(), []);
  const world = useMemo(() => engine.world, [engine]);
  const xOffsetRef = useRef(0);

  /* Keeps the ground underfoot, and "garbage collects" blocks that have exited the left side of the
     screen */
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
    drawScene(canvasContextRef.current, xOffsetRef.current, bodies);
    // On to the next frame
    requestRef.current = requestAnimationFrame(animate);
  }, [engine, world, viewportMoved]);

  // Initial setup
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

  return (
    <GameContext.Provider value={{ engine, world, canvasRef, canvasContextRef, xOffsetRef }}>
      <div className={cx('base')} style={{ width: `${BLOCKS_ACROSS * BLOCK_SIZE}px`, height: `${BLOCKS_DOWN * BLOCK_SIZE}px` }}>
        <canvas ref={canvasRef} className={cx('canvas')} />
      </div>
      {children}
    </GameContext.Provider>
  );
}

Game.propTypes = {
  children: PropTypes.node,
};
Game.defaultProps = {
  children: null,
};
