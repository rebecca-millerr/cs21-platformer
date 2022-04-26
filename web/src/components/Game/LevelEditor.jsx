import { useCallback, useEffect } from 'react';
import { useGameContext } from './Game';

import { BLOCK_SIZE } from './constants';

import Matter from 'matter-js';
import ColorHash from 'color-hash';

const colorHash = new ColorHash();

export default function LevelEditor() {
  const { canvasRef, world, xOffsetRef, ownId } = useGameContext();
  const playerColor = typeof ownId === 'number' ? colorHash.hex(ownId.toString()) : 'transparent';

  // Builders can place blocks
  const createBlock = useCallback((event) => {
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
      { isStatic: true, label: 'platform', render: { fillStyle: playerColor } },
    );
    // Check that it doesn't overlap anything in the world (runners, the ground, other blocks)
    const collisions = Matter.Query.collides(newBlock, Matter.Composite.allBodies(world));
    if (collisions.length > 0) return;
    // If it doesn't, add it to the world
    Matter.Composite.add(world, newBlock);
  }, [world, canvasRef, xOffsetRef, playerColor]);

  // Register event listener
  useEffect(() => {
    const canvas = canvasRef.current;
    canvas.addEventListener('mousedown', createBlock);
    return () => { canvas.removeEventListener('mousedown', createBlock); };
  }, [canvasRef, createBlock]);

  return null;
}
