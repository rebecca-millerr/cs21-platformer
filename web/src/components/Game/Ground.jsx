import { useRef, useEffect } from 'react';
import { useGameContext } from './Game';
import Matter from 'matter-js';

import { BLOCKS_ACROSS, BLOCKS_DOWN, BLOCK_SIZE } from './constants';

export default function Ground() {
  const { world, events, xOffsetRef } = useGameContext();

  // Create the ground and add it to the world
  const groundRef = useRef();
  useEffect(() => {
    // Create the ground
    groundRef.current = Matter.Bodies.rectangle(
      // x, y specify the *center* of the object
      (BLOCKS_ACROSS * BLOCK_SIZE) / 2,
      (BLOCKS_DOWN - 0.5) * BLOCK_SIZE,
      BLOCKS_ACROSS * BLOCK_SIZE,
      BLOCK_SIZE,
      { isStatic: true, label: 'ground', render: { fillStyle: '#15803d' } },
    );
    Matter.Composite.add(world, groundRef.current);

    // Keep it underfoot
    const moveGround = () => {
      const newX = (BLOCKS_ACROSS * BLOCK_SIZE) / 2 + xOffsetRef.current;
      Matter.Body.setPosition(groundRef.current, { x: newX, y: groundRef.current.position.y });
    };
    events.on('beforeFrame', moveGround);

    // Cleanup
    return () => {
      Matter.Composite.remove(world, groundRef.current);
      events.off('beforeFrame', moveGround);
    };
  }, [world, events, xOffsetRef]);

  return null;
}
