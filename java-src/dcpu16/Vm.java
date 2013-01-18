package dcpu16;

public class Vm{
		private int cycles = 0;
		private int[] registers = new int[8];
		private int[] ram = new int[0x1000];

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
}
