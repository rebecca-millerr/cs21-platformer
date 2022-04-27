import { CatmullRomCurve3, Vector2, Vector3 } from 'three';

// Given a streaming input of 2D points, can efficiently reconstruct a very smooth interpolated
// paths between them.

export default class StreamingInterpolator {
  constructor(buffer = 1000, curveType = 'centripetal') {
    this.anchors = [];
    this.buffer = buffer; // lag behind the values provided by the server in order to be smooth
    this.curveType = curveType;

    this._cachedPoints = [];
    this._cachedPointsCenter = 0;
  }

  addPoint(position) { // position should be [x, y]
    this.anchors.push({ position, time: Date.now() });
  }

  // Return a good estimate of the smoothed position that occured "buffer" milliseconds ago.
  // Returns null if there aren't at least two points earlier than and two points more recent than
  // the buffered time.
  getCurrentValue() {
    // The time we're trying to reconstruct is the current time minus the buffer
    const searchTime = Date.now() - this.buffer;
    // Find the first anchor in history that falls after the time we're trying to reconstruct.
    // We will base our curve on a set of anchorss loosely centered around this "just after" point
    // in history.
    const indexAfter = this.anchors.findIndex(({ time }) => time > searchTime);
    if (indexAfter === -1) return null;
    const timeAfter = this.anchors[indexAfter].time;

    // NOTE: We can cache the shape of the curve based on which anchor is this "just after the
    // search" anchor.

    // invalidate cached curve points
    if (timeAfter !== this._cachedPointsCenter) {
      const relevantIndices = [-2, -1, 0, 1].map((i) => indexAfter + i);
      const relevantAnchors = relevantIndices.map((i) => this.anchors[i]);

      const isGoodPoint = (p) => p && p.time && p.position[0] != null && p.position[1] != null;
      if (relevantAnchors.some((p) => !isGoodPoint(p))) return null;

      // Create the curve
      const curve = new CatmullRomCurve3(
        relevantAnchors.map(({ time, position }) => new Vector3(time, position[0], position[1])),
        false,
        this.curveType,
      );

      // Get enough points along the curve that we could have one for every 100ms (note that in
      // practice they won't line up like that)
      const timeSpan = relevantAnchors[3].time - relevantAnchors[0].time;
      const numPoints = Math.round(timeSpan / (this.buffer / 20));
      if (numPoints < 2) return null;
      this._cachedPoints = curve.getSpacedPoints(numPoints);
      this._cachedPointsCenter = timeAfter;

      // Clean up (remove points that are too old to be useful in the future)
      this.anchors = this.anchors.slice(relevantIndices[0]);
    }

    const curvePoints = this._cachedPoints;
    // Find one point on the smooth curve on either side of the time we're trying to reconstruct
    const afterOnCurveIdx = curvePoints.findIndex((p) => p.x > searchTime);
    if (afterOnCurveIdx < 1) return null;
    const afterOnCurve = curvePoints[afterOnCurveIdx];
    const beforeOnCurve = curvePoints[afterOnCurveIdx - 1];
    // Simple linear interpolation between those two points
    const a = new Vector2(beforeOnCurve.y, beforeOnCurve.z); // x is time
    const b = new Vector2(afterOnCurve.y, afterOnCurve.z);
    const t = (searchTime - beforeOnCurve.x) / (afterOnCurve.x - beforeOnCurve.x);
    return a.lerp(b, t);
  }

  // Like getCurrentValue, but when called repeatedly it will return the latest "good" value instead
  // of null when there aren't enough points to interpolate.
  getCurrentValueWithFallback(initialBest) {
    // We can provide a "seed" value to populate before we get *any* interpolated values
    this.latestValue ??= (initialBest && new Vector2(...initialBest));

    // Recompute but fall back if we need to
    this.latestValue = this.getCurrentValue() ?? this.latestValue;

    return this.latestValue;
  }
}
