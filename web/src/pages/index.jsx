import React from 'react';

import useStore from 'store';
import { useRouter } from 'next/router';

import classNames from 'classnames/bind';
import styles from './index.module.scss';
const cx = classNames.bind(styles);

export default function Homepage() {
  const router = useRouter();
  const setPlayerType = useStore((state) => state.setPlayerType);
  const setPlayerColor = useStore((state) => state.setPlayerColor);
  const join = (desiredPlayerType) => {
    const red = Math.floor(Math.random() * 255);
    const green = Math.floor(Math.random() * 255);
    const blue = Math.floor(Math.random() * 255);
    setPlayerColor({ red, green, blue });
    setPlayerType(desiredPlayerType);
    router.push('/game');
  };

  return (
    <div className={cx('base')}>
      <h1>Welcome to CS21 Platformer</h1>
      <button className={cx('join-cta')} type="button" onClick={() => join('runner')}>
        Join as a <strong>runner</strong>
      </button>
      <button className={cx('join-cta')} type="button" onClick={() => join('builder')}>
        Join as a <strong>builder</strong>
      </button>

      <div className={cx('credits')}>
        By <a href="https://github.com/rebecca-millerr">Becca</a>,{' '}
        <a href="https://github.com/tylerjcalabrese">Tyler</a>, and{' '}
        <a href="https://github.com/controversial">Luke</a> for Tufts CS 21: Concurrent Programming
      </div>
    </div>
  );
}
