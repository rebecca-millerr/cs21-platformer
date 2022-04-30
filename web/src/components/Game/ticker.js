// Provides smooth interpolated X position based on timekeeping "ticks" from the server

import { useState, useMemo, useRef, useEffect } from 'react';

import StreamingInterpolator from 'helpers/streaming-interpolation';

import { MOVING_SPEED } from './constants';
const TICKS_PER_SECOND = 8;

const ticksToX = (ticks) => (ticks / TICKS_PER_SECOND) * MOVING_SPEED;

export default function useInterpolatedXOffset(events) {
  const getter = useRef({ get: () => 0 });
  const interp = useMemo(() => new StreamingInterpolator(1500), []);

  // Update the getter to be functional once we get an initial value
  const [initialX, setInitialX] = useState(null);
  useEffect(() => {
    if (!initialX) return;
    getter.current.get = () => interp.getCurrentValueWithFallback([initialX, 0]).x;
  }, [interp, initialX]);


  useEffect(() => {
    const handleMessage = ({ ticks, blocks }) => {
      // the first message that we get when we join
      if (blocks && ticks) setInitialX(ticksToX(ticks));

      if (ticks) {
        const newX = (ticks / TICKS_PER_SECOND) * MOVING_SPEED;
        interp.addPoint([newX, 0]);
      }
    };
    events.on('socket_message', handleMessage);
    return () => events.off('socket_message', handleMessage);
  }, [events, interp]);

  return getter.current;
}
