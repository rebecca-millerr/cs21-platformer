import { useEffect } from 'react';
import { useGameContext } from './Game';

import { BLOCK_SIZE } from './constants';

import Matter from 'matter-js';

import colorFromId from 'helpers/color-from-id';


function getBlock(row, col, builder) {
  const color = typeof builder === 'number'
    ? colorFromId(builder)
    : 'transparent';

  return Matter.Bodies.rectangle(
    (col + 0.5) * BLOCK_SIZE,
    (row + 0.5) * BLOCK_SIZE,
    BLOCK_SIZE,
    BLOCK_SIZE,
    { isStatic: true, label: 'platform', render: { fillStyle: color } },
  );
}

export default function PeerBlocks() {
  const { events, world, playerType, ownId } = useGameContext();

  // Add new blocks from the websocket connection to the world
  useEffect(() => {
    const handleMessage = ({ blocks, newblock }) => {
      const newBlockSpecs = blocks ?? (newblock && [newblock]);
      if (!newBlockSpecs) return;

      const newBlocks = newBlockSpecs
        .filter(({ builder }) => playerType === 'runner' || builder !== ownId)
        .map(({ pos, builder }) => getBlock(pos.y, pos.x, builder));

      Matter.Composite.add(world, newBlocks);
    };

    events.on('socket_message', handleMessage);
    return () => { events.off('socket_message', handleMessage); };
  }, [events, world, ownId, playerType]);

  return null;
}
