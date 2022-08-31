#include <iostream>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

void quicksort(int *arr, int l, int r);
int part(int *arr, int l, int r);

int main() {
	int n;
	std::cin >> n;
	int *arr = new int[n]();
	for (int i = 0; i < n; i++) {
		std::cin >> arr[i];
	}
	quicksort(arr, 0, n - 1);
	for (int i = 0; i < n; i++) {
		std::cout << arr[i] << " ";
	}
	delete[] arr;
}

void quicksort(int *arr, int const l, int const r) {
	if (r > l) {
		int const k = part(arr, l, r);
		quicksort(arr, l, k);
		quicksort(arr, k + 1, r);
	}
}

int part(int *arr, int const l, int const r) {
	int const q = arr[(l + r) / 2];
	int left = l, right = r;
	while (right >= left) {
		while (arr[left] < q && q < arr[right]) {
			right--;
			left++;
		}
		while (arr[left] < q) {
			left++;
		}
		while (q < arr[right]) {
			right--;
		}
		if (right > left) {
			std::swap(arr[left++], arr[right--]);
		} else {
			break;
		}
	}
	return right;
}
