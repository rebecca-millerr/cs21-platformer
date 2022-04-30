import colors from 'tailwindcss/colors';

const hueCycle = [
  'rose', 'fuchsia', 'violet', 'blue', 'emerald', 'amber', 'pink',
];
const shadeCycle = [600, 300, 500, 900];

export default function colorFromId(id) {
  const hueIdx = id % hueCycle.length;
  const shadeIdx = Math.floor(id / hueCycle.length) % shadeCycle.length;
  return colors[hueCycle[hueIdx]]?.[shadeCycle[shadeIdx]] ?? '#000';
}
