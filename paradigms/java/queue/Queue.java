package queue;

import java.util.function.Predicate;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

	/*
		Model: ...a[n - 3], a[n - 2], a[n - 1], a[0], a[1], [2], ...
		head -> first element, tail -> last element
	*/

public interface Queue {
	/*
		Name: enqueue(element);
		Pred: element != null;
		Post: n' = n + 1 && a[n'] == element
	*/
	void enqueue(Object element);

	/*
		Name: dequeue();
		Pred: queue.head in [0..n - 1];
		Post: n' := queue.head && R <- a[n'];
	*/
	Object dequeue();

	/*
		Name: element();
		Pred: queue.head in [0..n - 1];
		Post: n' = queue.head && R = a[n'];
	*/
	Object element();

	/*
		Name: size()
		Pred: true
		Post: R <- queue.size
	*/
	int size();

	/*
		Name: isEmpty()
		Pred: queue != null && queue != null;
		Post: R <- queue.size <=> 0
	*/
	boolean isEmpty();

	/*
		Name: clear()
		Pred: true
		Post: R <- (forall i := [0..n - 1] : a[i] == null) && (queue.head := queue.tail := queue.tail := 0)
	*/
	void clear();

	/*
		Name: countIf()
		Pred: condition != null
		Post: R <- (forall i := [0..n - 1] : condition.test(a[i]) == true)
	*/
	int countIf(Predicate<Object> condition);
}
