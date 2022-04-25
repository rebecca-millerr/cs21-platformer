import create from 'zustand';

const useStore = create((set) => ({
  playerType: null, // null | 'runner' | 'builder'
  setPlayerType: (playerType) => {
    if (!['runner', 'builder'].includes(playerType)) throw new Error('Invalid player type');
    set(() => ({ playerType }));
  },
  playerColor: null, // null | <some string hex color>
  setPlayerColor: (playerColor) => {
    if (!playerColor.red || !playerColor.green || !playerColor.blue
        || Object.values(playerColor).some((val) => val < 0 || val > 255)) {
      throw new Error('Invalid player color');
    }
    set(() => ({ playerColor }));
  },
}));

export default useStore;
