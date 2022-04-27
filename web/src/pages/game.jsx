import React, { useState, useEffect } from 'react';

import Game, { Ground, ActiveRunner, LevelEditor, PeerBlocks, DebugPane, PeerRunners } from 'components/Game';

import useStore from 'store';
import { useRouter } from 'next/router';

import classNames from 'classnames/bind';
import styles from './game.module.scss';
const cx = classNames.bind(styles);

export default function GamePage() {
  const playerType = useStore((state) => state.playerType);

  // Kick out players that have no type (i.e. they navigated to the game page directly)
  const router = useRouter();
  useEffect(() => {
    if (!playerType) router.push('/');
  }, [router, playerType]);

  // Debug menu is a secret
  const [showDebug, setShowDebug] = useState(false);
  useEffect(() => {
    const onKeyDown = (e) => {
      if (e.key === 'd') setShowDebug(!showDebug);
    };
    window.addEventListener('keydown', onKeyDown);
    return () => window.removeEventListener('keydown', onKeyDown);
  }, [showDebug]);


  if (!playerType) return null;

  return (
    <div className={cx('base')}>
      <Game playerType={playerType}>
        {/* Blocks */}
        {playerType === 'builder' && <LevelEditor />}
        <PeerBlocks />
        <Ground />

        {/* Runners */}
        {playerType === 'runner' && <ActiveRunner />}
        <PeerRunners />

        {/* Debug info */}
        {showDebug && <DebugPane />}
      </Game>
    </div>
  );
}
