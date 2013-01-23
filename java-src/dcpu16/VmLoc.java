package dcpu16;

public class VmLoc{

		int val;
		int extraWord;
		/**
		 * Construct a VmLoc, passing in the 5/6 bit value indicator
		 * --may increase PC on the vm when getting value
		 **/
		public VmLoc(Vm vm, int val){
				this.val = val;
				if(val > 0x0f && val <= 0x17 ||
					 val == 0x1a ||
					 val == 0x1e ||
					 val == 0x1f){
						this.extraWord = vm.getNextWord();
				}
		}

		/**
		 * Sets this vmloc to the provided value in the provided vm
		 **/
		public void set(Vm vm, final int newValue){
				//Register
				if(val <= 0x07){
						vm.regSet(val, newValue);
				}
				//Register mem
				if(val <= 0x0f){
						vm.ramSet(vm.regGet(val - 0x08), newValue);
				}
				//Register mem + offset
				if(val <= 0x17){
						//TODO: this will cause the getNextWord to be called too many times...
						vm.ramSet(vm.regGet(val - 0x08) + extraWord, newValue);
				}
				//PUSH
				if(val == 0x18){
						vm.ramSet(vm.pushSP(), newValue);
				}
				//PEEK
				if(val == 0x19){
						vm.ramSet(vm.getSP(), newValue);
				}
				//PICK
				if(val == 0x1a){
						//TODO: this will cause the getNextWord to be called too many times...
						vm.ramSet(vm.getSP() + extraWord, newValue);
				}
				//SP
				if(val == 0x1b){
						vm.setSP(newValue);
				}
				//PC
				if(val == 0x1c){
						vm.setPC(newValue);
				}
				//EX
				if(val == 0x1d){
						vm.setEX(newValue);
				}
				//mem
				if(val == 0x1e){
						//TODO: this will cause the getNextWord to be called too many times...
						vm.ramSet(extraWord, newValue);
				}
				//next word lit
				if(val == 0x1f){
						//fail silently
				}
		}
		public int get(Vm vm){
				//Register
				if(val <= 0x07){
						return vm.regGet(val);
				}
				//Register mem
				if(val <= 0x0f){
						return vm.ramGet(vm.regGet(val - 0x08));
				}
				//Register mem + offset
				if(val <= 0x17){
						return vm.ramGet(vm.regGet(val - 0x10) + extraWord);
				}
				//Pop
				if(val == 0x18){
						return vm.ramGet(vm.popSP());
				}
				//Peek
				if(val == 0x19){
						return vm.ramGet(vm.getSP());
				}
				//Pick n
				if(val == 0x1a){
						return vm.ramGet(vm.getSP() + extraWord);
				}
				//SP
				if(val == 0x1b){
						return vm.getSP();
				}
				//PC
				if(val == 0x1c){
						return vm.getPC();
				}
				//EX
				if(val == 0x1d){
						return vm.getEX();
				}
				//Mem
				if(val == 0x1e){
						return vm.ramGet(extraWord);
				}
				//Literal next word
				if(val == 0x1f){
						return extraWord;
				}
				//Literal
				return val - 0x20;
		}
}
