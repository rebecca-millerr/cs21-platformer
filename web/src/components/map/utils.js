import _ from 'lodash';

import {
  BLOCKS_ACROSS,
  BLOCKS_DOWN,
  DEFAULT_BLOCK,
  EMPTY,
  VISIBLE_BLOCKS_ACROSS,
} from './constants';

// converts row and col to position in 1d array
export function getBlockIndex(row, col) {
  return row * VISIBLE_BLOCKS_ACROSS + col;
}

// TODO: have this run once total, not once per client - not sure how to do this
// creates map where all spaces besides bottom row are empty and has
// spare row on right
export function getDefaultMap() {
  const map = [];
  for (let row = 0; row < BLOCKS_DOWN - 1; row += 1) {
    for (let col = 0; col < VISIBLE_BLOCKS_ACROSS; col += 1) {
      map.push({
        display: EMPTY,
        absoluteCol: col,
      });
    }
  }
  for (
    let colAtBottom = 0;
    colAtBottom < VISIBLE_BLOCKS_ACROSS;
    colAtBottom += 1
  ) {
    map.push({
      display: DEFAULT_BLOCK,
      absoluteCol: colAtBottom,
    });
  }
  return map;
}

// returns given map with everything rotated one column to left, and
// new default col on the far right
export function addDefaultColumn(map) {
  const mapClone = _.cloneDeep(map); // doesn't mutate original map
  const lastAbsoluteCol = mapClone[getBlockIndex(0, BLOCKS_ACROSS)].absoluteCol;

  for (let row = 0; row < BLOCKS_DOWN; row += 1) {
    for (let col = 0; col < BLOCKS_ACROSS; col += 1) {
      // take value of block to the right
      mapClone[getBlockIndex(row, col)] = mapClone[getBlockIndex(row, col + 1)];
    }
    // make rightmost col look like default map
    mapClone[getBlockIndex(row, BLOCKS_ACROSS)] = {
      display: row === BLOCKS_DOWN - 1 ? DEFAULT_BLOCK : EMPTY,
      absoluteCol: lastAbsoluteCol + 1,
    };
  }
  return mapClone;
}
