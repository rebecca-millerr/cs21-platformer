// Abuses perfect-cursors for smooth interpolation
// Synchronizes with timekeeping "ticks" from the server

import { useRef, useMemo, useEffect } from 'react';
import { PerfectCursor as Interpolator } from 'perfect-cursors';
import { MOVING_SPEED } from './constants';

const TICKS_PER_SECOND = 8;

export default function useInterpolatedXOffset(events) {
  const xOffsetRef = useRef(null);
  const pc = useMemo(() => new Interpolator(([x]) => { xOffsetRef.current = x; }), []);

  useEffect(() => {
    const update = ({ ticks }) => {
      if (ticks) pc.addPoint([(ticks / TICKS_PER_SECOND) * MOVING_SPEED, 0]);
    };
    events.on('socket_message', update);
    return () => events.off('socket_message', update);
  }, [events, pc]);

  return xOffsetRef;
}
