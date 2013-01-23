package dcpu16;

import dcpu16.VmLoc;

public class Vm{
		private int cycles = 0;
		private int pc = 0;
		private int ex = 0;
		private int sp = 0xffff;
		private int ia = 0;
		private int[] registers = new int[8];
		private int[] ram = new int[0x1000];

		private VmLoc aloc = new VmLoc();
		private VmLoc bloc = new VmLoc();

		private static int getShort(final int num){
				if(num < 0){
						return (Math.abs(num) + (1 << 15)) & 0xFFFF;
				}
				return num & 0xFFFF;
		}
		public int ramGet(final int loc){
				incCycles();
				return getShort(ram[loc]);
		}
		public void ramSet(final int loc, final int val){
				incCycles();
				ram[loc] = getShort(val);
		}

		public void regSet(final int reg, final int val){
				registers[reg] = getShort(val);
		}
		public int regGet(final int reg){
				return getShort(registers[reg]);
		}

		public void incCycles(){
				cycles++;
		}
		public int getCycles(){
				return cycles;
		}
		/**
		 * Returns the next word of code ([PC++])
		 **/
		public int getNextWord(){
				return getShort(ramGet(pc++));
		}
		public int getSP(){
				return getShort(sp);
		}
		public void setSP(int newVal){
				sp = newVal;
		}
		public int popSP(){
				return sp++;
		}
		public int pushSP(){
				return --sp;
		}
		public int getPC(){
				return getShort(pc);
		}
		public void setPC(int newVal){
				pc = newVal;
		}
		public int getEX(){
				return getShort(ex);
		}
		public void setEX(int newVal){
				ex = newVal;
		}
}
