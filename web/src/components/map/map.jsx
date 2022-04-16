import React, { useEffect } from 'react';
import PropTypes from 'prop-types';

import { getBlockIndex, addDefaultColumn } from './utils';
import {
  BLOCKS_ACROSS,
  BLOCKS_DOWN,
  BLOCK_SIZE,
  MOVING_SPEED,
} from './constants';
import styles from './map.module.css';

export default function Map({ map, updateMap, time }) {
  useEffect(() => {
    const canvas = document.getElementById('map-canvas');
    const context = canvas.getContext('2d');
    context.clearRect(0, 0, canvas.width, canvas.height);

    function fillBlock(colStart, rowStart, color) {
      context.fillStyle = color;
      context.fillRect(colStart, rowStart, BLOCK_SIZE, BLOCK_SIZE);

      context.beginPath();
      context.rect(colStart, rowStart, BLOCK_SIZE, BLOCK_SIZE);
      context.stroke();
    }

    const adjustedX = Math.floor(time / MOVING_SPEED) % BLOCK_SIZE;

    for (let row = 0; row < canvas.height; row += BLOCK_SIZE) {
      const pixelRow = Math.floor(row / BLOCK_SIZE);
      for (let col = 0 - adjustedX; col < canvas.width; col += BLOCK_SIZE) {
        const pixelCol = Math.floor(col / BLOCK_SIZE) + 1;
        fillBlock(col, row, map[getBlockIndex(pixelRow, pixelCol)].display);
      }
    }

    // if entering brand new col
    if (adjustedX === 0) {
      updateMap(addDefaultColumn(map));
    }

    // don't check for map because then everything is double-triggered,
    // and map is updated when time updates anyway
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [time, updateMap]);

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
