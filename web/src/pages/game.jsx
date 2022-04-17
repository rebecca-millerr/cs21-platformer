import React, { useEffect } from 'react';

import Game from 'components/Game';
import useStore from 'store';
import { useRouter } from 'next/router';

export default function GamePage() {
  const playerType = useStore((state) => state.playerType);

  // Kick out players that have no type (i.e. they navigated to the game page directly)
  const router = useRouter();
  useEffect(() => {
    if (!playerType) router.push('/');
  }, [router, playerType]);
  if (!playerType) return null;

  return (
    <div>
      Player type is {playerType}
      <br />
      <Game allowBuilding={playerType === 'builder'} createRunner={playerType === 'runner'} />
    </div>
  );
}
