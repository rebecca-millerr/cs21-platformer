/* Graphical representation of a runner */

// Load sprite

const spriteSheet = typeof window !== 'undefined' ? new Image() : null;
if (spriteSheet) spriteSheet.src = './assets/sprites/character.png';

// How big is each sprite
const spriteWidth = 50;
const spriteHeight = 37;
// How are sprites laid out on the page?
const sheetWidth = 7;

export const sequences = {
  idle: { seq: [0, 1, 2, 3], loop: true },
  crouch: { seq: [4, 5, 6, 7], loop: true },
  run: { seq: [8, 9, 10, 11, 12, 13], loop: true },
  jump: { seq: [14, 15, 16, 17], loop: false },
  somersault: { seq: [18, 19, 20, 21], loop: true },
  fall: { seq: [22, 23], loop: true },
};


export function getCoordinates(spriteNumber) {
  const x = (spriteNumber % sheetWidth) * spriteWidth;
  const y = Math.floor(spriteNumber / sheetWidth) * spriteHeight;
  return { x, y, width: spriteWidth, height: spriteHeight };
}


export default class RunnerSprite {
  constructor(state = 'idle') {
    this.state = state;
    this._counter = 0;
    this.lastFrameTime = Date.now();
    this.width = spriteWidth;
    this.height = spriteHeight;
    this.direction = 1;
  }

  setState(state) {
    this.state = state;
    this._counter = 0;
  }

  nextFrame() {
    this.lastFrameTime = Date.now();

    this._counter += 1;
    if (sequences[this.state].loop) {
      this._counter %= sequences[this.state].seq.length;
    } else {
      this._counter = Math.min(this._counter, sequences[this.state].seq.length - 1);
    }
  }

  tick() {
    const frameDelay = 200;
    if (Date.now() - this.lastFrameTime > frameDelay) this.nextFrame();
  }

  getFrame() {
    return sequences[this.state].seq[this._counter];
  }

  getSpriteCoordinates() {
    return getCoordinates(this.getFrame());
  }

  setDirection(direction) {
    if (![-1, 1].includes(direction)) return;
    this.direction = direction;
  }

  getCanvas(setDirection = null) {
    if (setDirection) this.direction = setDirection;

    const { x, y, width, height } = this.getSpriteCoordinates();
    const canvas = document.createElement('canvas');
    canvas.width = width;
    canvas.height = height;
    const ctx = canvas.getContext('2d');
    ctx.imageSmoothingEnabled = false;
    if (this.direction === -1) ctx.translate(width, 0);
    ctx.scale(this.direction, 1);
    ctx.drawImage(
      spriteSheet,
      x, y, width, height, // positions on sprite sheet
      0, 0, width, height, // positions on canvas
    );
    return canvas;
  }
}
