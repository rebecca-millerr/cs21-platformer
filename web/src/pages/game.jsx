import React, { useCallback, useEffect, useRef, useState } from 'react';

import Map, { getDefaultMap } from 'components/map';

export default function Game() {
  // TODO: magically gets updates from erlang server and populates
  //       them in state, which updates map
  const [map, setMap] = useState(getDefaultMap());

  // Use useRef for mutable variables that we want to persist
  // without triggering a re-render on their change
  const requestRef = useRef();
  const previousTimeRef = useRef();

  const animate = useCallback((time) => {
    if (previousTimeRef.current !== undefined) {
      // Pass on a function to the setter of the state
      // to make sure we always have the latest state
      // TODO: actually replace this state with update from server
      setMap((prevMap) => prevMap);
    }
    previousTimeRef.current = time;
    requestRef.current = requestAnimationFrame(animate);
  }, []);

  useEffect(() => {
    requestRef.current = requestAnimationFrame(animate);
    return () => cancelAnimationFrame(requestRef.current);
  }, [animate]);

  return (
    <div>
      <Map map={map} />
    </div>
  );
}
