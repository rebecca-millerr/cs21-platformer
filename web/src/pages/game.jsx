import React from 'react';

import Map from 'components/map';
import useStore from 'store';

export default function Game() {
  const playerType = useStore((state) => state.playerType);

  return (
    <div>
      Player type is {playerType}
      <br />
      <Map />
    </div>
  );
}
