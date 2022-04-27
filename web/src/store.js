import create from 'zustand';

const useStore = create((set) => ({
  playerType: null, // null | 'runner' | 'builder'
  setPlayerType: (playerType) => {
    if (!['runner', 'builder'].includes(playerType)) throw new Error('Invalid player type');
    set(() => ({ playerType }));
  },
}));

export default useStore;
