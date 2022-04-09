import React, { useEffect } from 'react';
import PropTypes from 'prop-types';

const BLOCKS_ACROSS = 40;
const BLOCKS_DOWN = 30;
const BLOCK_SIZE = 20;

// TODO: have other types/colors dynamically named after builders
const DEFAULT_BLOCK = '#354585';
const EMPTY = '#ffffff';

export default function Map({ map }) {
  useEffect(() => {
    const canvas = document.getElementById('map-canvas');
    const context = canvas.getContext('2d');

    for (let row = 0; row < canvas.height; row += BLOCK_SIZE) {
      for (let col = 0; col < canvas.width; col += BLOCK_SIZE) {
        const pixelRow = row / BLOCK_SIZE;
        const pixelCol = col / BLOCK_SIZE;
        context.fillStyle = map[pixelRow * BLOCKS_ACROSS + pixelCol];
        context.fillRect(col, row, BLOCK_SIZE, BLOCK_SIZE);

        context.beginPath();
        context.rect(col, row, BLOCK_SIZE, BLOCK_SIZE);
        context.stroke();
      }
    }
  }, [map]);

  return (
    <>
      <canvas
        width={BLOCKS_ACROSS * BLOCK_SIZE}
        height={BLOCKS_DOWN * BLOCK_SIZE}
        id="map-canvas"
      />
    </>
  );
}

// creates map where all spaces besides bottom row are empty
export function getDefaultMap() {
  const map = [];
  for (let row = 0; row < BLOCKS_DOWN - 1; row += 1) {
    for (let col = 0; col < BLOCKS_ACROSS; col += 1) {
      map.push(EMPTY);
    }
  }
  for (let colAtBottom = 0; colAtBottom < BLOCKS_ACROSS; colAtBottom += 1) {
    map.push(DEFAULT_BLOCK);
  }
  return map;
}

Map.propTypes = {
  map: PropTypes.array.isRequired,
};
