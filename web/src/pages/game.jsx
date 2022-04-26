import React, { useEffect } from 'react';

import Game, { Ground, ActiveRunner, LevelEditor, DebugPane } from 'components/Game';

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
  if (!playerType) return null;

  return (
    <div className={cx('base')}>
      <Game playerType={playerType}>
        <Ground />
        {playerType === 'runner' && <ActiveRunner />}
        {playerType === 'builder' && <LevelEditor />}
        {/* TODO: <OtherRunners /> */}
        {/* TODO: <OtherBlocks /> */}
        <DebugPane />
      </Game>
    </div>
  );
}
