package md2html;

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class Markdown {
    private final List<String> process;
    private final StringBuilder sb;

    private static final Map<String, LibraryPairs> MARKDOWN_LIBRARY;
    private static final Map<String, String> SPECIAL_LIBRARY;
    private static final HashSet<String> MARKDOWN_ELEMENTS = new HashSet<>();

    private final Map<String, Integer> count;

    static {
        MARKDOWN_ELEMENTS.add("*");
        MARKDOWN_ELEMENTS.add("**");
        MARKDOWN_ELEMENTS.add("_");
        MARKDOWN_ELEMENTS.add("__");
        MARKDOWN_ELEMENTS.add("--");
        MARKDOWN_ELEMENTS.add("-");
        MARKDOWN_ELEMENTS.add("`");
        MARKDOWN_ELEMENTS.add("%");
        MARKDOWN_LIBRARY = Map.of(
                "*", new LibraryPairs("<em>", "</em>"),
                "_", new LibraryPairs("<em>", "</em>"),
                "**", new LibraryPairs("<strong>", "</strong>"),
                "__", new LibraryPairs("<strong>", "</strong>"),
                "--", new LibraryPairs("<s>", "</s>"),
                "`", new LibraryPairs("<code>", "</code>"),
                "%", new LibraryPairs("<var>", "</var>")
        );
        SPECIAL_LIBRARY = Map.of(
                "<", "&lt;",
                ">", "&gt;",
                "&", "&amp;"
        );
    }

    public Markdown(final List<String> input) {
        this.process = input;
        this.sb = new StringBuilder();
        this.count = new LinkedHashMap<>();
        this.count.put("*", 2);
        this.count.put("_", 2);
        this.count.put("**", 2);
        this.count.put("__", 2);
        this.count.put("--", 2);
        this.count.put("`", 2);
        this.count.put("%", 2);
        this.count();
    }

    private void count() {
        for (int i = 0; i < this.process.size(); i++) {
            boolean flag = false;
            boolean skip = false;
            boolean last = false;
            final String s = this.process.get(i);
            this.sb.setLength(0);
            for (int j = 0; j < s.length() - 1; j++) {
                final String key = String.valueOf(s.charAt(j));
                if (MARKDOWN_ELEMENTS.contains(key)) {
                    if (!flag) {
                        if (!skip) {
                            if (j - 1 >= 0) {
                                if (s.charAt(j - 1) == '\\') {
                                    flag = true;
                                }
                            }
                            final String s1 = s.charAt(j) + "" + s.charAt(j + 1);
                            if (MARKDOWN_ELEMENTS.contains(s1)) {
                                if (!flag) {
                                    if (j + 2 < s.length()) {
                                        if (!Character.isWhitespace(s.charAt(j + 2))) {
                                            this.sb.append(this.count.get(s1) % 2 == 0 ? MARKDOWN_LIBRARY.get(s1).getStart() : MARKDOWN_LIBRARY.get(s1).getEnd());
                                            this.count.put(s1, this.count.get(s1) % 2 == 0 ? 1 : 2);
                                            last = true;
                                        } else {
                                            if (j - 1 >= 0) {
                                                if (!Character.isWhitespace(s.charAt(j - 1))) {
                                                    this.sb.append(this.count.get(s1) % 2 == 0 ? MARKDOWN_LIBRARY.get(s1).getStart() : MARKDOWN_LIBRARY.get(s1).getEnd());
                                                    this.count.put(s1, this.count.get(s1) % 2 == 0 ? 1 : 2);
                                                    last = true;
                                                } else {
                                                    this.sb.append(s1);
                                                    last = false;
                                                }
                                            } else {
                                                if (this.count.get(s1) % 2 != 0) {
                                                    this.sb.append(MARKDOWN_LIBRARY.get(s1).getEnd());
                                                    this.count.put(s1, 2);
                                                    last = true;
                                                } else {
                                                    this.sb.append(s1);
                                                }
                                            }

                                        }
                                    } else {
                                        if (this.count.get(s1) % 2 != 0) {
                                            this.sb.append(MARKDOWN_LIBRARY.get(s1).getEnd());
                                            this.count.put(s1, 2);
                                            last = true;
                                        } else {
                                            this.sb.append(s1);
                                            last = false;
                                        }
                                    }
                                } else {
                                    this.sb.append(s1);
                                    last = false;
                                }
                                skip = true;
                            } else {
                                if (!flag) {
                                    if (j + 1 < s.length()) {
                                        if (!Character.isWhitespace(s.charAt(j + 1))) {
                                            if (this.count.containsKey(key)) {
                                                this.sb.append(this.count.get(key) % 2 == 0 ? MARKDOWN_LIBRARY.get(key).getStart() : MARKDOWN_LIBRARY.get(key).getEnd());
                                                this.count.put(key, this.count.get(key) % 2 == 0 ? 1 : 2);
                                                last = true;
                                            } else {
                                                this.sb.append(key);
                                                last = false;
                                            }
                                        } else {
                                            if (j - 1 >= 0) {
                                                if (!Character.isWhitespace(s.charAt(j - 1))) {
                                                    if (this.count.containsKey(key)) {
                                                        this.sb.append(this.count.get(key) % 2 == 0 ? MARKDOWN_LIBRARY.get(key).getStart() : MARKDOWN_LIBRARY.get(key).getEnd());
                                                        this.count.put(key, this.count.get(key) % 2 == 0 ? 1 : 2);
                                                        last = true;
                                                    } else {
                                                        this.sb.append(key);
                                                        last = false;
                                                    }
                                                } else {
                                                    this.sb.append(key);
                                                    last = false;
                                                }
                                            } else {
                                                if (this.count.get(key) % 2 != 0) {
                                                    this.sb.append(MARKDOWN_LIBRARY.get(key).getEnd());
                                                    this.count.put(key, 2);
                                                    last = true;
                                                } else {
                                                    this.sb.append(key);
                                                }
                                            }
                                        }
                                    } else {
                                        if (this.count.get(key) % 2 != 0) {
                                            this.sb.append(MARKDOWN_LIBRARY.get(key).getEnd());
                                            this.count.put(key, 2);
                                            last = true;
                                        } else {
                                            this.sb.append(key);
                                            last = false;
                                        }
                                    }
                                } else {
                                    this.sb.append(key);
                                    last = false;
                                    flag = false;
                                }
                            }
                        } else {
                            skip = false;
                        }
                    } else {
                        flag = false;
                    }
                } else {
                    if (((j - 1) >= 0) && SPECIAL_LIBRARY.containsKey((key))) {
                        this.sb.append(SPECIAL_LIBRARY.get((key)));
                    } else {
                        if (key.charAt(0) != '\\') {
                            this.sb.append(key);
                        }
                    }
                    skip = false;
                    last = false;
                }
            }
            if (!MARKDOWN_ELEMENTS.contains(String.valueOf(s.charAt(s.length() - 1)))) {
                this.sb.append(s.charAt(s.length() - 1));
            } else {
                if (!last) {
                    boolean temp = false;
                    temp = exceptionChecking(temp, s);
                    if (!temp) {
                        this.sb.append(s.charAt(s.length() - 1));
                    }
                }
            }
            this.process.set(i, sb.toString());
        }
    }

    private boolean exceptionChecking(boolean temp, final String s) {
        if (s.charAt(s.length() - 1) == '`' && this.count.containsKey("`") && this.count.get("`") % 2 != 0) {
            this.sb.append(MARKDOWN_LIBRARY.get("`").getEnd());
            this.count.put("`", this.count.get("`") % 2 == 0 ? 1 : 2);
            temp = true;
        }
        if (s.charAt(s.length() - 1) == '%' && this.count.containsKey("%") && this.count.get("%") % 2 != 0) {
            this.sb.append(MARKDOWN_LIBRARY.get("%").getEnd());
            this.count.put("%", this.count.get("%") % 2 == 0 ? 1 : 2);
            temp = true;
        }
        final char c = s.charAt(s.length() - 2);
        if (s.charAt(s.length() - 1) == '*' && this.count.containsKey("*") && this.count.get("*") % 2 != 0 && c != '*') {
            this.sb.append(MARKDOWN_LIBRARY.get("*").getEnd());
            this.count.put("*", this.count.get("*") % 2 == 0 ? 1 : 2);
            temp = true;
        }
        if (s.charAt(s.length() - 1) == '_' && this.count.containsKey("_") && this.count.get("_") % 2 != 0 && c != '_') {
            this.sb.append(MARKDOWN_LIBRARY.get("_").getEnd());
            this.count.put("_", this.count.get("_") % 2 == 0 ? 1 : 2);
            temp = true;
        }
        if (s.charAt(s.length() - 1) == '%' && c == '\\') {
            this.sb.append("%");
            this.count.put("%", this.count.get("%") % 2 == 0 ? 1 : 2);
            temp = true;
        }
        return temp;
    }

    public List<String> getList() {
        return this.process;
    }

    static class LibraryPairs {
        private final String start;
        private final String end;

        public LibraryPairs(String start, String end) {
            this.start = start;
            this.end = end;
        }

        public String getStart() {
            return this.start;
        }

        public String getEnd() {
            return this.end;
        }
    }
}
