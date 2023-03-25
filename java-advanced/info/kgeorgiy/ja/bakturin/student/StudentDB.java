package info.kgeorgiy.ja.bakturin.student;

import info.kgeorgiy.java.advanced.student.GroupName;
import info.kgeorgiy.java.advanced.student.Student;
import info.kgeorgiy.java.advanced.student.StudentQuery;

import java.util.*;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class StudentDB implements StudentQuery {
	private static final Comparator<Student> NAME_COMPARATOR = Comparator.comparing(Student::getLastName, Comparator.reverseOrder())
			.thenComparing(Student::getFirstName, Comparator.reverseOrder())
			.thenComparing(Student::getId);

	/* "atom" methods */
	private static <T> Stream<T> get(final Stream<Student> students, final Function<? super Student, ? extends T> getter) {
		return students.map(getter);
	}

	private static <T> Stream<T> get(final Collection<Student> students, final Function<? super Student, ? extends T> getter) {
		return get(students.stream(), getter);
	}

	private static <T> List<T> getToList(final Collection<Student> students, final Function<? super Student, T> getter) {
		return get(students, getter).toList();
	}

	private static Stream<Student> sort(final Stream<Student> students) {
		return students.sorted(NAME_COMPARATOR);
	}

	private static <T> Stream<T> sort(final Stream<T> students, final Comparator<T> comparator) {
		return students.sorted(comparator);
	}

	private static <T> Stream<T> sort(final Collection<T> students, final Comparator<T> comparator) {
		return sort(students.stream(), comparator);
	}

	private static List<Student> sortToList(final Stream<Student> students) {
		return sort(students).toList();
	}

	private static List<Student> sortToList(final Collection<Student> students) {
		return sortToList(students.stream());
	}

	private static <T> List<T> sortToList(final Stream<T> students, final Comparator<T> comparator) {
		return sort(students, comparator).toList();
	}

	private static List<Student> sortToList(final Collection<Student> students, final Comparator<Student> comparator) {
		return sortToList(students.stream(), comparator);
	}

	private static <T> Stream<Student> find(final Stream<Student> students, final T toFind, final Function<? super Student, ? extends T> getter) {
		return students.filter(s -> getter.apply(s).equals(toFind));
	}

	private static <T> Stream<Student> find(final Collection<Student> students, final T toFind, final Function<? super Student, ? extends T> getter) {
		return find(students.stream(), toFind, getter);
	}

	/* methods for preventing copy-paste */
	private static Stream<String> getFirstNamesStream(final Stream<Student> students) {
		return get(students, Student::getFirstName);
	}

	private static Stream<String> getFirstNamesStream(final Collection<Student> students) {
		return getFirstNamesStream(students.stream());
	}

	private static Stream<Student> findStudentsByGroupStream(final Stream<Student> students, final GroupName group) {
		return find(students, group, Student::getGroup);
	}

	private static Stream<Student> findStudentsByGroupStream(final Collection<Student> students, final GroupName group) {
		return findStudentsByGroupStream(students.stream(), group);
	}

	/* interface methods */
	@Override
	public List<String> getFirstNames(final List<Student> students) {
		return getToList(students, Student::getFirstName);
	}

	@Override
	public List<String> getLastNames(final List<Student> students) {
		return getToList(students, Student::getLastName);
	}

	@Override
	public List<GroupName> getGroups(final List<Student> students) {
		return getToList(students, Student::getGroup);
	}

	@Override
	public List<String> getFullNames(final List<Student> students) {
		return getToList(students, s -> s.getFirstName() + " " + s.getLastName());
	}

	@Override
	public Set<String> getDistinctFirstNames(final List<Student> students) {
		return getFirstNamesStream(students).collect(Collectors.toCollection(TreeSet::new));
	}

	@Override
	public String getMaxStudentFirstName(final List<Student> students) {
		return getFirstNamesStream(sort(students, Comparator.reverseOrder()))
				.findFirst()
				.orElse("");
	}

	@Override
	public List<Student> sortStudentsById(final Collection<Student> students) {
		return sortToList(students, Comparator.naturalOrder());
	}

	@Override
	public List<Student> sortStudentsByName(final Collection<Student> students) {
		return sortToList(students);
	}

	@Override
	public List<Student> findStudentsByFirstName(final Collection<Student> students, final String name) {
		return sortToList(find(students, name, Student::getFirstName));
	}

	@Override
	public List<Student> findStudentsByLastName(final Collection<Student> students, final String name) {
		return sortToList(find(students, name, Student::getLastName));
	}

	@Override
	public List<Student> findStudentsByGroup(final Collection<Student> students, final GroupName group) {
		return sortToList(findStudentsByGroupStream(students, group));
	}

	@Override
	public Map<String, String> findStudentNamesByGroup(final Collection<Student> students, final GroupName group) {
		return findStudentsByGroupStream(students, group)
				.collect(Collectors.toMap(Student::getLastName, Student::getFirstName, BinaryOperator.minBy(Comparable::compareTo)));
	}
}
