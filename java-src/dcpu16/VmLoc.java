package dcpu16;

public class VmLoc{

		int val;
		int extraWord;
		Vm vm;
		/**
		 * Construct a VmLoc
		 **/
		public VmLoc(Vm vm){
				this.vm = vm;
		}
		/**
		 * load a new VmLoc from the VM
		 * passing in the 5/6 bit value indicator
		 * --may increase PC on the vm when getting value
		 **/
		public void load(final int newVal){
				this.val = newVal;
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
		public void set(final int newValue){
				//Register
				if(val <= 0x07){
						vm.regSet(val, newValue);
						return;
				}
				//Register mem
				if(val <= 0x0f){
						vm.ramSet(vm.regGet(val - 0x08), newValue);
						return;
				}
				//Register mem + offset
				if(val <= 0x17){
						//TODO: this will cause the getNextWord to be called too many times...
						vm.ramSet(vm.regGet(val - 0x08) + extraWord, newValue);
						return;
				}
				//PUSH
				if(val == 0x18){
						vm.ramSet(vm.pushSP(), newValue);
						return;
				}
				//PEEK
				if(val == 0x19){
						vm.ramSet(vm.getSP(), newValue);
						return;
				}
				//PICK
				if(val == 0x1a){
						//TODO: this will cause the getNextWord to be called too many times...
						vm.ramSet(vm.getSP() + extraWord, newValue);
						return;
				}
				//SP
				if(val == 0x1b){
						vm.setSP(newValue);
						return;
				}
				//PC
				if(val == 0x1c){
						vm.setPC(newValue);
						return;
				}
				//EX
				if(val == 0x1d){
						vm.setEX(newValue);
						return;
				}
				//mem
				if(val == 0x1e){
						//TODO: this will cause the getNextWord to be called too many times...
						vm.ramSet(extraWord, newValue);
						return;
				}
				//next word lit
				if(val == 0x1f){
						//fail silently
						return;
				}
		}
		public int get(){
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
