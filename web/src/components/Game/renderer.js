import { useState, useMemo } from 'react';
import { BLOCKS_DOWN, BLOCKS_ACROSS, BLOCK_SIZE } from './constants';
import Matter from 'matter-js';

function _renderBody(body, { canvasContextRef, xOffsetRef }) {
  const canvasContext = canvasContextRef.current;
  const xOffset = xOffsetRef.current;

  canvasContext.beginPath();
  body.vertices.forEach(({ x, y }) => canvasContext.lineTo(x - xOffset, y));
  canvasContext.closePath();
  canvasContext.fillStyle = body.render.fillStyle ?? '#000';
  canvasContext.fill();
}

export function renderGround({ world, ...gameContext }) {
  Matter.Composite.allBodies(world).forEach((body) => {
    if (body.label !== 'ground') return;
    _renderBody(body, gameContext);
  });
}

export function renderPlatforms({ world, ...gameContext }) {
  Matter.Composite.allBodies(world).forEach((body) => {
    if (body.label !== 'platform') return;
    _renderBody(body, gameContext);
  });
}

export function renderRunners({ canvasContextRef, xOffsetRef, world }) {
  const canvasContext = canvasContextRef.current;
  const xOffset = xOffsetRef.current;

  Matter.Composite.allBodies(world).forEach((body) => {
    if (body.label !== 'runner') return;
    const { x, y } = body.position;
    const { runner } = body.render;
    const renderedRunner = runner.getCanvas();
    const { width, height } = renderedRunner;
    canvasContext.drawImage(renderedRunner, x - xOffset - (width / 2), y - (height / 2));
    runner.tick();
  });
}


export function renderGrid({ canvasContextRef, xOffsetRef }) {
  const canvasContext = canvasContextRef.current;
  const xOffset = xOffsetRef.current;

  // Latitude lines
  canvasContext.strokeStyle = 'rgba(0, 0, 0, 0.25)';
  for (let y = 0; y <= BLOCKS_DOWN; y += 1) {
    canvasContext.beginPath();
    canvasContext.moveTo(0, y * BLOCK_SIZE);
    canvasContext.lineTo(BLOCKS_ACROSS * BLOCK_SIZE, y * BLOCK_SIZE);
    canvasContext.stroke();
  }

  // Longitude lines
  const firstLongitudeLine = Math.floor(xOffset / BLOCK_SIZE);
  const lastLongitudeLine = firstLongitudeLine + BLOCKS_ACROSS + 1;
  for (let x = firstLongitudeLine; x <= lastLongitudeLine; x += 1) {
    canvasContext.beginPath();
    canvasContext.moveTo(x * BLOCK_SIZE - xOffset, 0);
    canvasContext.lineTo(x * BLOCK_SIZE - xOffset, BLOCKS_DOWN * BLOCK_SIZE);
    canvasContext.stroke();
  }
}

class Renderer {
  constructor(passes = []) {
    this.passes = passes;
  }

  addPass(pass) {
    this.passes.push(pass);
    // cleanup function
    return () => {
      this.passes = this.passes.filter((p) => p !== pass);
    };
  }

  draw(gameContext) {
    const canvasContext = gameContext.canvasContextRef.current;
    canvasContext.clearRect(0, 0, canvasContext.canvas.width, canvasContext.canvas.height);
    this.passes.forEach((pass) => pass(gameContext));
  }
}

export default function useRenderer() {
  return useMemo(() => new Renderer([
    renderGround,
    renderPlatforms,
    renderGrid,
    renderRunners,
  ]), []);
}
