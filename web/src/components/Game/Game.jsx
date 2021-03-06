import React, {
  useMemo, useCallback, useRef, useEffect, createContext, useContext,
} from 'react';
import PropTypes from 'prop-types';

import { BLOCKS_ACROSS, BLOCKS_DOWN, BLOCK_SIZE } from './constants';
import Matter from 'matter-js';
import mitt from 'mitt';
import useRenderer from './renderer';
import useSocketConnection from './socket';
import useInterpolatedXOffset from './ticker';

import classNames from 'classnames/bind';
import styles from './Game.module.scss';
const cx = classNames.bind(styles);

export const GameContext = createContext();
export const useGameContext = () => useContext(GameContext);


export default function Game({ children, playerType }) {
  const canvasRef = useRef();
  const canvasContextRef = useRef();

  const engine = useMemo(() => Matter.Engine.create(), []);
  const world = useMemo(() => engine.world, [engine]);
  const events = useMemo(() => mitt(), []);
  const { socket, ownId } = useSocketConnection(playerType, events);

  const interpolatedXOffset = useInterpolatedXOffset(events);
  const xOffsetRef = useRef(null);

  /* eslint-disable object-property-newline */
  const gameContext = useMemo(
    () => ({
      engine, world, xOffsetRef, // Physics world
      ownId, playerType, // Player identity
      events, socket, // Connections
      canvasRef, canvasContextRef, // Drawing
    }),
    [engine, world, events, socket, ownId, playerType],
  );
  /* eslint-enable */

  // Render loop
  const requestRef = useRef();
  const previousTimeRef = useRef();
  const renderer = useRenderer();
  const animate = useCallback((time) => {
    // Timekeeping
    const delta = (time && previousTimeRef.current)
      ? (time - previousTimeRef.current) // ms
      : 0; // no delta if no previous time (i.e. on first render)
    previousTimeRef.current = time;

    const oldXOffset = xOffsetRef.current;
    xOffsetRef.current = interpolatedXOffset.get();
    if (oldXOffset === 0 && xOffsetRef.current !== 0) {
      events.emit('positionFound');
    }

    events.emit('beforeFrame', { delta });

    // Run physics simulation
    // If more than 1 / 60 of a second has elapsed, run the simulation in little
    // steps
    const maxDelta = 1000 / 60;
    // subSteps > 0 iff more than 1/60 secs elapsed; if delta < maxDelta the
    // loop doesn't run
    const steps = Math.floor(delta / maxDelta);
    for (let i = 0; i < steps; i += 1) Matter.Engine.update(engine, maxDelta);
    // Get over the finish line to the proper delta we're trying to reach
    Matter.Engine.update(engine, (delta - (steps * maxDelta)));

    // Paint the picture
    renderer.draw(gameContext);

    events.emit('afterFrame', { delta });

    // On to the next frame
    requestRef.current = requestAnimationFrame(animate);
  }, [gameContext, engine, renderer, events, interpolatedXOffset]);

  // Initial setup
  useEffect(() => {
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
    return () => cancelAnimationFrame(requestRef.current);
  }, [world, animate]);

  // Garbage collect blocks that have exited the screen
  useEffect(() => {
    const collectGarbage = () => {
      Matter.Composite.allBodies(world).forEach((body) => {
        const { vertices } = body;
        const leftEdge = xOffsetRef.current;
        // Every vertex is off the left edge of the screen by at least
        // BLOCK_SIZE pixels
        if (vertices.every(({ x }) => x < leftEdge - BLOCK_SIZE)) {
          Matter.Composite.remove(world, body);
        }
      });
    };
    events.on('afterFrame', collectGarbage);
    return () => events.off('afterFrame', collectGarbage);
  }, [world, events, xOffsetRef]);


  const contextValue = useMemo(
    () => ({ ...gameContext, renderer }),
    [gameContext, renderer],
  );
  return (
    <GameContext.Provider value={contextValue}>
      <div className={cx('base')}>
        <canvas
          ref={canvasRef}
          className={cx('canvas')}
          style={{
            width: `${BLOCKS_ACROSS * BLOCK_SIZE}px`,
            height: `${BLOCKS_DOWN * BLOCK_SIZE}px`,
          }}
        />
        {children}
      </div>
    </GameContext.Provider>
  );
}

Game.propTypes = {
  children: PropTypes.node,
  playerType: PropTypes.oneOf(['runner', 'builder']).isRequired,
};
Game.defaultProps = {
  children: null,
};
