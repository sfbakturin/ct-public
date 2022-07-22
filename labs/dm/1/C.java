import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class C {
    public static void main(final String... args) {
        Scanner in = new Scanner(System.in);
        byte n = in.nextByte();
        in.nextLine();
        int[][] inputDesc = new int[32][];
        int[][] inputArgs = new int[32][];
        byte[] speed1 = new byte[32];
        byte[] speed2 = new byte[32];
        Map<Integer, Byte> argsIndex = new LinkedHashMap<>();
        Map<Integer, Byte> funcIndex = new LinkedHashMap<>();
        ArrayList<Byte> indexes = new ArrayList<>();
        StringBuilder sb = new StringBuilder();
        boolean start = false;
        byte loop = 0;
        byte kA = 0;
        byte kF = 0;
        byte idx = 1;
        byte index_Desc = 0;
        byte index_Args = 0;
        byte index_argsIndex = 0;
        byte index_funcIndex = 0;
        byte size = 0;
        while (true) {
            String[] s = in.nextLine().split(" ");
            if (!start) {
                if (s.length == 1) {
                    loop++;
                    kA++;
                    argsIndex.put((int) idx, index_argsIndex);
                    speed1[idx] = index_argsIndex;
                    idx++;
                    index_argsIndex++;
                    if (loop == n) {
                        break;
                    }
                } else {
                    loop++;
                    kF++;
                    start = true;
                    funcIndex.put((int) idx, index_funcIndex);
                    speed2[idx] = index_funcIndex;
                    indexes.add(idx);
                    index_funcIndex++;
                    if (s[0].compareTo("1") == 0) {
                        inputDesc[index_Desc] = new int[2];
                        inputDesc[index_Desc][0] = 1;
                        inputDesc[index_Desc][1] = Integer.parseInt(s[1]);
                        size = 1;
                    }

                    if (s[0].compareTo("2") == 0) {
                        inputDesc[index_Desc] = new int[3];
                        inputDesc[index_Desc][0] = 2;
                        inputDesc[index_Desc][1] = Integer.parseInt(s[1]);
                        inputDesc[index_Desc][2] = Integer.parseInt(s[2]);
                        size = 2;
                    }

                    if (s[0].compareTo("3") == 0) {
                        inputDesc[index_Desc] = new int[4];
                        inputDesc[index_Desc][0] = 3;
                        inputDesc[index_Desc][1] = Integer.parseInt(s[1]);
                        inputDesc[index_Desc][2] = Integer.parseInt(s[2]);
                        inputDesc[index_Desc][3] = Integer.parseInt(s[3]);
                        size = 3;
                    }

                    if (s[0].compareTo("4") == 0) {
                        inputDesc[index_Desc] = new int[5];
                        inputDesc[index_Desc][0] = 4;
                        inputDesc[index_Desc][1] = Integer.parseInt(s[1]);
                        inputDesc[index_Desc][2] = Integer.parseInt(s[2]);
                        inputDesc[index_Desc][3] = Integer.parseInt(s[3]);
                        inputDesc[index_Desc][4] = Integer.parseInt(s[4]);
                        size = 4;
                    }

                    if (s[0].compareTo("5") == 0) {
                        inputDesc[index_Desc] = new int[6];
                        inputDesc[index_Desc][0] = 5;
                        inputDesc[index_Desc][1] = Integer.parseInt(s[1]);
                        inputDesc[index_Desc][2] = Integer.parseInt(s[2]);
                        inputDesc[index_Desc][3] = Integer.parseInt(s[3]);
                        inputDesc[index_Desc][4] = Integer.parseInt(s[4]);
                        inputDesc[index_Desc][5] = Integer.parseInt(s[5]);
                        size = 5;
                    }
                    index_Desc++;
                    idx++;
                }
            } else {
                start = false;
                inputArgs[index_Args] = new int[(int) Math.pow(2, size)];
                size = 0;
                for (byte i = 0; i < s.length; i++) {
                    inputArgs[index_Args][i] = Integer.parseInt(s[i]);
                }
                index_Args++;
                if (loop == n) {
                    break;
                }
            }
        }
        in.close();
        byte[][] x = new byte[(int) Math.pow(2, kA)][kF + 1];
        idx = 0;
        for (int i = 0; i < kF; i++) {
            if (inputDesc[i][0] == 1) {
                for (int j = 0; j < 1 << kA; j++) {
                    StringBuilder s = new StringBuilder(Integer.toBinaryString(j));
                    StringBuilder st = new StringBuilder();
                    while (s.length() != kA) {
                        s.insert(0, "0");
                    }
                    if (argsIndex.containsKey(inputDesc[i][1])) {
                        st.append(s.charAt(speed1[inputDesc[i][1]]));
                    } else {
                        st.append(x[j][speed2[inputDesc[i][1]]]);
                    }

                    byte b = (byte) inputArgs[i][Integer.parseInt(st.toString(), 2)];
                    x[j][idx] = b;
                    sb.append(b);
                }
            }
            if (inputDesc[i][0] == 2) {
                for (int j = 0; j < 1 << kA; j++) {
                    StringBuilder s = new StringBuilder(Integer.toBinaryString(j));
                    StringBuilder st = new StringBuilder();
                    while (s.length() != kA) {
                        s.insert(0, "0");
                    }
                    if (argsIndex.containsKey(inputDesc[i][1])) {
                        st.append(s.charAt(speed1[inputDesc[i][1]]));
                    } else {
                        st.append(x[j][speed2[inputDesc[i][1]]]);
                    }

                    if (argsIndex.containsKey(inputDesc[i][2])) {
                        st.append(s.charAt(speed1[inputDesc[i][2]]));
                    } else {
                        st.append(x[j][speed2[inputDesc[i][2]]]);
                    }

                    byte b = (byte) inputArgs[i][Integer.parseInt(st.toString(), 2)];
                    x[j][idx] = b;
                    sb.append(b);
                }
            }
            if (inputDesc[i][0] == 3) {
                for (int j = 0; j < 1 << kA; j++) {
                    StringBuilder s = new StringBuilder(Integer.toBinaryString(j));
                    StringBuilder st = new StringBuilder();
                    while (s.length() != kA) {
                        s.insert(0, "0");
                    }
                    if (argsIndex.containsKey(inputDesc[i][1])) {
                        st.append(s.charAt(speed1[inputDesc[i][1]]));
                    } else {
                        st.append(x[j][speed2[inputDesc[i][1]]]);
                    }

                    if (argsIndex.containsKey(inputDesc[i][2])) {
                        st.append(s.charAt(speed1[inputDesc[i][2]]));
                    } else {
                        st.append(x[j][speed2[inputDesc[i][2]]]);
                    }

                    if (argsIndex.containsKey(inputDesc[i][3])) {
                        st.append(s.charAt(speed1[inputDesc[i][3]]));
                    } else {
                        st.append(x[j][speed2[inputDesc[i][3]]]);
                    }

                    byte b = (byte) inputArgs[i][Integer.parseInt(st.toString(), 2)];
                    x[j][idx] = b;
                    sb.append(b);
                }
            }
            if (inputDesc[i][0] == 4) {
                for (int j = 0; j < 1 << kA; j++) {
                    StringBuilder s = new StringBuilder(Integer.toBinaryString(j));
                    StringBuilder st = new StringBuilder();
                    while (s.length() != kA) {
                        s.insert(0, "0");
                    }
                    if (argsIndex.containsKey(inputDesc[i][1])) {
                        st.append(s.charAt(speed1[inputDesc[i][1]]));
                    } else {
                        st.append(x[j][speed2[inputDesc[i][1]]]);
                    }

                    if (argsIndex.containsKey(inputDesc[i][2])) {
                        st.append(s.charAt(speed1[inputDesc[i][2]]));
                    } else {
                        st.append(x[j][speed2[inputDesc[i][2]]]);
                    }

                    if (argsIndex.containsKey(inputDesc[i][3])) {
                        st.append(s.charAt(speed1[inputDesc[i][3]]));
                    } else {
                        st.append(x[j][speed2[inputDesc[i][3]]]);
                    }

                    if (argsIndex.containsKey(inputDesc[i][4])) {
                        st.append(s.charAt(speed1[inputDesc[i][4]]));
                    } else {
                        st.append(x[j][speed2[inputDesc[i][4]]]);
                    }

                    byte b = (byte) inputArgs[i][Integer.parseInt(st.toString(), 2)];
                    x[j][idx] = b;
                    sb.append(b);
                }
            }
            if (inputDesc[i][0] == 5) {
                for (int j = 0; j < 1 << kA; j++) {
                    StringBuilder s = new StringBuilder(Integer.toBinaryString(j));
                    StringBuilder st = new StringBuilder();
                    while (s.length() != kA) {
                        s.insert(0, "0");
                    }
                    if (argsIndex.containsKey(inputDesc[i][1])) {
                        st.append(s.charAt(speed1[inputDesc[i][1]]));
                    } else {
                        st.append(x[j][speed2[inputDesc[i][1]]]);
                    }

                    if (argsIndex.containsKey(inputDesc[i][2])) {
                        st.append(s.charAt(speed1[inputDesc[i][2]]));
                    } else {
                        st.append(x[j][speed2[inputDesc[i][2]]]);
                    }

                    if (argsIndex.containsKey(inputDesc[i][3])) {
                        st.append(s.charAt(speed1[inputDesc[i][3]]));
                    } else {
                        st.append(x[j][speed2[inputDesc[i][3]]]);
                    }

                    if (argsIndex.containsKey(inputDesc[i][4])) {
                        st.append(s.charAt(speed1[inputDesc[i][4]]));
                    } else {
                        st.append(x[j][speed2[inputDesc[i][4]]]);
                    }

                    if (argsIndex.containsKey(inputDesc[i][5])) {
                        st.append(s.charAt(speed1[inputDesc[i][5]]));
                    } else {
                        st.append(x[j][speed2[inputDesc[i][5]]]);
                    }

                    byte b = (byte) inputArgs[i][Integer.parseInt(st.toString(), 2)];
                    x[j][idx] = b;
                    sb.append(b);
                }
            }
            idx++;
            if (i + 1 != kF) {
                sb.setLength(0);
            }
        }
        byte[] paths = new byte[32];
        int last = -1;
        int u = 0;
        for (Byte index : indexes) {
            int depth = 0;
            for (int j = 1; j < inputDesc[u].length; j++) {
                if (!argsIndex.containsKey(inputDesc[u][j])) {
                    depth = Math.max(depth, paths[inputDesc[u][j]]);
                }
            }
            byte b = (byte) (depth + 1);
            paths[index] = b;
            last = index;
            u++;
        }
        System.out.println(paths[last]);
        System.out.println(sb);
    }
}
