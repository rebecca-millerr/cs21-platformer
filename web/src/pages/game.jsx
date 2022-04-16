import React, { useCallback, useEffect, useRef, useState } from 'react';

import { Map, getDefaultMap } from 'components/map';
import useStore from 'store';
import { useRouter } from 'next/router';

export default function Game() {
  // TODO: magically gets updates from erlang server and populates
  //       them in state, which updates map
  const [map, setMap] = useState(getDefaultMap());
  const [timeElapsed, setTimeElapsed] = useState(0);

  const requestRef = useRef();
  const previousTimeRef = useRef();

  const animate = useCallback((time) => {
    if (previousTimeRef.current !== undefined) {
      // TODO: actually replace this state with update from server
      setMap((prevMap) => [...prevMap]);
    }
    previousTimeRef.current = time;
    setTimeElapsed(time);
    requestRef.current = requestAnimationFrame(animate);
  }, []);

  useEffect(() => {
    requestRef.current = requestAnimationFrame(animate);
    return () => cancelAnimationFrame(requestRef.current);
  }, [animate]);

  const playerType = useStore((state) => state.playerType);
  const router = useRouter();
  useEffect(() => {
    if (!playerType) {
      router.push('/');
    }
  }, [router, playerType]);

  if (!playerType) return null;

  return (
    <div>
      Player type is {playerType}
      <br />
      {/* TODO: update server state when col added to map */}
      <Map map={map} updateMap={setMap} time={timeElapsed} />
    </div>
  );
}
