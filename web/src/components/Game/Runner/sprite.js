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
  idle: { seq: [0, 1, 2, 3], loop: true, pace: 5 },
  crouch: { seq: [4, 5, 6, 7], loop: true, pace: 10 },
  run: { seq: [8, 9, 10, 11, 12, 13], loop: true, pace: 10 },
  jump: { seq: [14, 15, 16, 17], loop: false, pace: 10 },
  somersault: { seq: [18, 19, 20, 21], loop: true, pace: 10 },
  fall: { seq: [22, 23], loop: true, pace: 5 },
};


export function getCoordinates(spriteNumber) {
  const x = (spriteNumber % sheetWidth) * spriteWidth;
  const y = Math.floor(spriteNumber / sheetWidth) * spriteHeight;
  return { x, y, width: spriteWidth, height: spriteHeight };
}


export default class RunnerSprite {
  constructor(size = spriteHeight, state = 'idle') {
    this.state = state;
    this._counter = 0;
    this.lastFrameTime = Date.now();

    this.direction = 1;

    this._canvas = document.createElement('canvas');
    this._canvas.width = Math.ceil((size / spriteHeight) * spriteWidth);
    this._canvas.height = size;

    this.width = this._canvas.width;
    this.height = this._canvas.height;
  }

  setState(state) {
    if (this.state === state) return;
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
    const frameDelay = 1000 / sequences[this.state].pace;
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

  getCanvas() {
    const { x, y, width, height } = this.getSpriteCoordinates();
    const ctx = this._canvas.getContext('2d');
    ctx.clearRect(0, 0, this.width, this.height);
    ctx.imageSmoothingEnabled = false;
    ctx.save();
    if (this.direction === -1) ctx.translate(this.width, 0);
    ctx.scale(this.direction, 1);
    ctx.drawImage(
      spriteSheet,
      x, y, width, height, // positions on sprite sheet
      0, 0, this.width, this.height, // positions on canvas
    );
    ctx.restore();
    return this._canvas;
  }
}
