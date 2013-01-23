package dcpu16;

public class Vm{
		private int cycles = 0;
		private int pc = 0;
		private int ex = 0;
		private int sp = 0xffff;
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
		public int getNextWord(){
				return getShort(ramGet(++pc));
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
		/**
		 * Gets the value requested from the code word
		 * -- May increase PC while getting next word --
		 * @param
		 *   val - the 5/6 bit value indicator
		 **/
		private int getVal(int val){
				//Register
				if(val <= 0x07){
						return regGet(val);
				}
				//Register mem
				if(val <= 0x0f){
						return ramGet(regGet(val - 0x08));
				}
				//Register mem + offset
				if(val <= 0x17){
						return ramGet(regGet(val - 0x10) + getNextWord());
				}
				//Pop
				if(val == 0x18){
						return ramGet(sp++);
				}
				//Peek
				if(val == 0x19){
						return ramGet(sp);
				}
				//Pick n
				if(val == 0x1a){
						return ramGet(sp + getNextWord());
				}
				//SP
				if(val == 0x1b){
						return sp;
				}
				//PC
				if(val == 0x1c){
						return pc;
				}
				//EX
				if(val == 0x1d){
						return ex;
				}
				//Mem
				if(val == 0x1e){
						return ramGet(getNextWord());
				}
				//Literal next word
				if(val == 0x1f){
						return getNextWord();
				}
				//Literal
				return val - 0x20;
		}
		/**
		 * sets a code value (b-value)
		 * @params
		 *  val - the 5-bit value to store to
		 *  newValue - the new value to set
		 **/
		private void setVal(int val, int newValue){
				//Register
				if(val <= 0x07){
						regSet(val, newValue);
				}
				//Register mem
				if(val <= 0x0f){
						ramSet(regGet(val - 0x08), newValue);
				}
				//Register mem + offset
				if(val <= 0x17){
						//TODO: this will cause the getNextWord to be called too many times...
						ramSet(regGet(val - 0x08) + getNextWord(), newValue);
				}
				//PUSH
				if(val == 0x18){
						ramSet(--sp, newValue);
				}
				//PEEK
				if(val == 0x19){
						ramSet(sp, newValue);
				}
				//PICK
				if(val == 0x1a){
						//TODO: this will cause the getNextWord to be called too many times...
						ramSet(sp + getNextWord(), newValue);
				}
				//SP
				if(val == 0x1b){
						sp = newValue;
				}
				//PC
				if(val == 0x1c){
						pc = newValue;
				}
				//EX
				if(val == 0x1d){
						ex = newValue;
				}
				//mem
				if(val == 0x1e){
						//TODO: this will cause the getNextWord to be called too many times...
						ramSet(getNextWord(), newValue);
				}
				//next word lit
				if(val == 0x1f){
						//fail silently
				}
		}
}
