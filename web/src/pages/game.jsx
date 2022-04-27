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

      {playerType === 'builder' && (
        <div className={cx('instructions')}>
          <span>Click to place blocks</span>
        </div>
      )}

      {playerType === 'runner' && (
        <div className={cx('instructions')}>
          <div>Use arrow keys to move</div>

          <div>
            <kbd>
              <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor">
                <path fillRule="evenodd" d="M9.707 14.707a1 1 0 01-1.414 0l-4-4a1 1 0 010-1.414l4-4a1 1 0 011.414 1.414L7.414 9H15a1 1 0 110 2H7.414l2.293 2.293a1 1 0 010 1.414z" clipRule="evenodd" />
              </svg>
            </kbd>
            <kbd>
              <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor">
                <path fillRule="evenodd" d="M10.293 5.293a1 1 0 011.414 0l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414-1.414L12.586 11H5a1 1 0 110-2h7.586l-2.293-2.293a1 1 0 010-1.414z" clipRule="evenodd" />
              </svg>
            </kbd>
            <kbd>
              <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor">
                <path fillRule="evenodd" d="M5.293 9.707a1 1 0 010-1.414l4-4a1 1 0 011.414 0l4 4a1 1 0 01-1.414 1.414L11 7.414V15a1 1 0 11-2 0V7.414L6.707 9.707a1 1 0 01-1.414 0z" clipRule="evenodd" />
              </svg>
            </kbd>
          </div>
        </div>
      )}
    </div>
  );
}
