import React, { useEffect, useState } from 'react';
import PropTypes from 'prop-types';
import _ from 'lodash';

import { getBlockIndex, addDefaultColumn } from './utils';
import {
  BLOCKS_ACROSS,
  BLOCKS_DOWN,
  BLOCK_SIZE,
  MOVING_SPEED,
  VISIBLE_BLOCKS_ACROSS,
} from './constants';
import styles from './map.module.css';

// TODO: replace this with actual random color for current user
const CURR_BUILDER_COLOR = '#c934eb';

export default function Map({ map, updateMap, time }) {
  const [deltaX, setDeltaX] = useState(0);

  function fillBlock(colStart, rowStart, color, context) {
    context.fillStyle = color;
    context.fillRect(colStart, rowStart, BLOCK_SIZE, BLOCK_SIZE);

    context.beginPath();
    context.rect(colStart, rowStart, BLOCK_SIZE, BLOCK_SIZE);
    context.stroke();
  }

  // updates drawing on canvas based on current map
  useEffect(() => {
    const canvas = document.getElementById('map-canvas');
    const context = canvas.getContext('2d');
    context.clearRect(0, 0, canvas.width, canvas.height);

    const adjustedX = Math.floor(time / MOVING_SPEED) % BLOCK_SIZE;

    for (let row = 0; row < canvas.height; row += BLOCK_SIZE) {
      const pixelRow = Math.floor(row / BLOCK_SIZE);
      for (let col = 0 - adjustedX; col < canvas.width; col += BLOCK_SIZE) {
        const pixelCol = Math.floor(col / BLOCK_SIZE) + 1;
        fillBlock(
          col,
          row,
          map[getBlockIndex(pixelRow, pixelCol)].display,
          context
        );
      }
    }

    // if entering brand new col
    if (adjustedX === 0) {
      updateMap(addDefaultColumn(map));
    }
    setDeltaX(adjustedX);

    // don't check for map because then everything is double-triggered,
    // and map is updated when time updates anyway
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [time, updateMap]);

  // listens for builder interactions to update map
  useEffect(() => {
    const getMousePosition = (event) => {
      const canvas = document.getElementById('map-canvas');
      const rect = canvas.getBoundingClientRect();
      const { top, right, bottom, left } = rect;

      const targetX = event.clientX;
      const targetY = event.clientY;

      // vertically flipped for some reason
      const x = targetX > left && targetX < right ? targetX - left : null;
      const y = targetY > top && targetY < bottom ? targetY - top : null;

      // if clicked within the image
      if (x != null && y != null) {
        const row = Math.floor((y / canvas.height) * BLOCKS_DOWN);
        const col = Math.floor((x / canvas.width) * VISIBLE_BLOCKS_ACROSS);

        const { absoluteCol } = map[getBlockIndex(row, col)];
        // TODO: replace this with cast to server to update with this
        console.log(absoluteCol);

        // TODO: this bugs out if it skips zero a little
        const tempMap = _.cloneDeep(map);
        tempMap[getBlockIndex(row, col)].display = CURR_BUILDER_COLOR;
        updateMap(tempMap);

        const context = canvas.getContext('2d');
        fillBlock(
          col * BLOCK_SIZE - deltaX,
          row * BLOCK_SIZE,
          CURR_BUILDER_COLOR,
          context
        );
      }
    };

    window.addEventListener('mousedown', getMousePosition);

    return () => window.removeEventListener('mousedown', getMousePosition);
  }, [map, deltaX, updateMap]);

  return (
    <canvas
      width={BLOCKS_ACROSS * BLOCK_SIZE}
      height={BLOCKS_DOWN * BLOCK_SIZE}
      id="map-canvas"
      className={styles.mapCanvas}
    />
  );
}

Map.propTypes = {
  map: PropTypes.arrayOf(
    PropTypes.shape({
      display: PropTypes.string,
      absoluteCol: PropTypes.number,
    })
  ).isRequired,
  updateMap: PropTypes.func.isRequired,
  time: PropTypes.number,
};
Map.defaultProps = {
  time: 0,
};
