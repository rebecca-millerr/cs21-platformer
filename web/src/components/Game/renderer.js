import { BLOCKS_DOWN, BLOCKS_ACROSS, BLOCK_SIZE } from './constants';

export function renderBlocks(canvasContext, offset, blockBodies) {
  // Draw bodies
  blockBodies.forEach((body) => {
    // if (body.label === 'runner') return;

    canvasContext.beginPath();
    body.vertices.forEach(({ x, y }) => canvasContext.lineTo(x - offset, y));
    canvasContext.closePath();
    canvasContext.fillStyle = body.render.fillStyle ?? '#000';
    canvasContext.fill();
  });
}

export function renderRunners(canvasContext, offset, runnerBodies) {
  // Draw runners
  runnerBodies.filter((body) => body.label === 'runner').forEach((body) => {
    const { x, y } = body.position;
    const { runner } = body.render;
    const renderedRunner = runner.getCanvas();
    const { width, height } = renderedRunner;
    canvasContext.drawImage(renderedRunner, x - offset - (width / 2), y - (height / 2));
    runner.tick();
  });
}

export function renderGrid(canvasContext, offset) {
  // Latitude lines
  canvasContext.strokeStyle = 'rgba(0, 0, 0, 0.25)';
  for (let y = 0; y <= BLOCKS_DOWN; y += 1) {
    canvasContext.beginPath();
    canvasContext.moveTo(0, y * BLOCK_SIZE);
    canvasContext.lineTo(BLOCKS_ACROSS * BLOCK_SIZE, y * BLOCK_SIZE);
    canvasContext.stroke();
  }

  // Longitude lines
  const firstLongitudeLine = Math.floor(offset / BLOCK_SIZE);
  const lastLongitudeLine = firstLongitudeLine + BLOCKS_ACROSS + 1;
  for (let x = firstLongitudeLine; x <= lastLongitudeLine; x += 1) {
    canvasContext.beginPath();
    canvasContext.moveTo(x * BLOCK_SIZE - offset, 0);
    canvasContext.lineTo(x * BLOCK_SIZE - offset, BLOCKS_DOWN * BLOCK_SIZE);
    canvasContext.stroke();
  }
}

export default function drawScene(canvasContext, offset, bodies) {
  canvasContext.clearRect(0, 0, canvasContext.canvas.width, canvasContext.canvas.height);

  renderBlocks(canvasContext, offset, bodies.filter((body) => ['platform', 'ground'].includes(body.label)));
  renderGrid(canvasContext, offset);
  renderRunners(canvasContext, offset, bodies.filter((body) => body.label === 'runner'));
}
