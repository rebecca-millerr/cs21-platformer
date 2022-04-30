/* Physical body that models a runner and can respond to keyboard events */

import Matter from 'matter-js';
import RunnerSprite from './sprite';

export default class Runner {
  constructor(world, startingX) {
    this.world = world;
    this.sprite = new RunnerSprite();
    this.body = Matter.Bodies.rectangle(
      startingX, 100, this.sprite.width * 0.33, this.sprite.height,
      {
        label: 'active-runner',
        render: { sprite: this.sprite },
      },
    );
    Matter.Body.setInertia(this.body, Infinity);
    Matter.Composite.add(world, this.body);

    this.leftPressed = false;
    this.rightPressed = false;
    this.upPressed = false;
  }

  remove() {
    Matter.Composite.remove(this.world, this.body);
  }

  onKeyDown(e) {
    const { key } = e;
    if (key.startsWith('Arrow')) e.preventDefault();
    if (key === 'ArrowLeft') this.leftPressed = true;
    else if (key === 'ArrowRight') this.rightPressed = true;
    else if (key === 'ArrowUp') this.upPressed = true;
  }

  onKeyUp(e) {
    const { key } = e;
    if (key === 'ArrowLeft') this.leftPressed = false;
    else if (key === 'ArrowRight') this.rightPressed = false;
    else if (key === 'ArrowUp') this.upPressed = false;
  }

  isStanding() {
    const runnerBounds = Matter.Bounds.create(this.body.vertices);
    const detectionBounds = {
      min: { x: runnerBounds.min.x, y: runnerBounds.max.y },
      max: { x: runnerBounds.max.x, y: runnerBounds.max.y + 1 },
    };
    const bodies = Matter.Composite.allBodies(this.world);
    const bodiesStandingOn = Matter.Query.region(bodies, detectionBounds)
      .filter((body) => ['ground', 'platform'].includes(body.label));
    return bodiesStandingOn.length > 0;
  }

  // run every frame
  update() {
    const movingLeft = this.leftPressed && !this.rightPressed;
    const movingRight = this.rightPressed && !this.leftPressed;
    const standing = this.isStanding();

    // Player movement

    // Moving left / right
    if (movingLeft) {
      Matter.Body.setVelocity(this.body, { x: -2, y: this.body.velocity.y });
    }
    if (movingRight) {
      Matter.Body.setVelocity(this.body, { x: 2, y: this.body.velocity.y });
    }
    // jumping
    if (this.upPressed && standing) {
      Matter.Body.setVelocity(this.body, { x: this.body.velocity.x, y: -5 });
    }

    // Update sprite
    if (movingLeft) this.sprite.setDirection(-1);
    if (movingRight) this.sprite.setDirection(1);

    if (!standing) {
      if (this.body.velocity.y < -0.1) this.sprite.setState('jump');
      else if (this.body.velocity.y > 0.1) this.sprite.setState('fall');
    } else if (movingLeft || movingRight) {
      this.sprite.setState('run');
    } else {
      this.sprite.setState('idle');
    }

    this.sprite.tick();
  }
}
