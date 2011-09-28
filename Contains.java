package foo;

public class Contains {

	// False - returns incorrect in even occurrences...
	public boolean myContains(int[] bar, int x){
		boolean flag = false;
		for(int i=0; i < bar.length; i++){
			if (bar[i]==x) flag = !flag;
		}
		return flag;
	}
}
