/* Physical body that models a runner */

import Matter from 'matter-js';
import RunnerSprite from './sprite';

export default class Runner {
  constructor(world) {
    this.world = world;
    this.sprite = new RunnerSprite();
    this.body = Matter.Bodies.rectangle(
      400, 100, this.sprite.width - 25, this.sprite.height,
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

  jump() {
    // Jumping is not allowed if we're not standing on a platform
    const runnerBounds = Matter.Bounds.create(this.body.vertices);
    const platformDetectionBounds = {
      min: { x: runnerBounds.min.x, y: runnerBounds.max.y },
      max: { x: runnerBounds.max.x, y: runnerBounds.max.y + 1 },
    };
    const bodies = Matter.Composite.allBodies(this.world);
    const bodiesStandingOn = Matter.Query.region(bodies, platformDetectionBounds)
      .filter((body) => ['ground', 'platform'].includes(body.label));
    const isStandingOnPlatform = bodiesStandingOn.length > 0;
    if (!isStandingOnPlatform) return;

    Matter.Body.setVelocity(this.body, { x: this.body.velocity.x, y: -5 });
  }

  // run every frame
  update() {
    // Moving left / right
    if (this.rightPressed && !this.leftPressed) {
      Matter.Body.setVelocity(this.body, { x: 2, y: this.body.velocity.y });
      this.sprite.setDirection(1);
    }
    if (this.leftPressed && !this.rightPressed) {
      Matter.Body.setVelocity(this.body, { x: -2, y: this.body.velocity.y });
      this.sprite.setDirection(-1);
    }
    // jumping
    if (this.upPressed) {
      this.jump();
    }
  }
}
