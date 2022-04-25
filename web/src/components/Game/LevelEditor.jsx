import { useCallback, useEffect } from 'react';
import { useGameContext } from './Game';

import { BLOCK_SIZE } from './constants';

import Matter from 'matter-js';
import useStore from 'store';

export default function LevelEditor() {
  const { canvasRef, world, xOffsetRef } = useGameContext();
  const playerColor = useStore((state) => state.playerColor);

  // Builders can place blocks
  const createBlock = useCallback((event) => {
    // Find out where in the world they clicked
    const { top, left } = canvasRef.current.getBoundingClientRect();
    const x = event.clientX - left + xOffsetRef.current;
    const y = event.clientY - top;
    // Create a block for that space
    const row = Math.floor(y / BLOCK_SIZE);
    const col = Math.floor(x / BLOCK_SIZE);
    const color = `rgb(${playerColor.red}, ${playerColor.green}, ${playerColor.blue})`;
    const newBlock = Matter.Bodies.rectangle(
      (col + 0.5) * BLOCK_SIZE,
      (row + 0.5) * BLOCK_SIZE,
      BLOCK_SIZE,
      BLOCK_SIZE,
      { isStatic: true, label: 'platform', render: { fillStyle: color } },
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
