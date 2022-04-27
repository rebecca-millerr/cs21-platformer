import { useEffect } from 'react';
import { useGameContext } from './Game';

import { BLOCK_SIZE } from './constants';

import Matter from 'matter-js';
import ColorHash from 'color-hash';

const colorHash = new ColorHash();

function getBlock(row, col, builder) {
  const color = typeof builder === 'number' ? colorHash.hex(builder.toString()) : 'transparent';

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
    const handleMessage = (message) => {
      const newBlockSpecs = message.blocks ?? (message.newblock && [message.newblock]);
      if (!newBlockSpecs) return;

      const newBlocks = newBlockSpecs
        .filter(({ builder }) => playerType === 'runner' || builder !== ownId)
        .map(({ pos, builder }) => getBlock(pos.y, pos.x, builder));

      Matter.Composite.add(world, newBlocks);
      console.log(`added ${newBlocks.length} blocks from server`);
    };

    events.on('socket_message', handleMessage);
    return () => { events.off('socket_message', handleMessage); };
  }, [events, world, ownId, playerType]);

  return null;
}
