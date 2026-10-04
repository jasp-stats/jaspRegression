#include <cpp11.hpp>  // doubles, integers, writable::integers
#include <vector>     // std::vector
#include <algorithm>  // std::stable_sort, std::sort
#include <numeric>    // std::iota
#include <cstring>    // std::memset

using namespace cpp11;

static inline int signum(double x) {
	return (x > 0) - (x < 0);
}

// Naive O(n^2) concordance: sign(xj - xi) * sign(yj - yi) for each i
[[cpp11::register]]
integers concordance_naive_cpp(doubles x, doubles y) {
	int n = x.size();
	writable::integers out(n);

	for (int i = 0; i < n; i++) {
		int s = 0;
		for (int j = 0; j < n; j++)
			if (i != j)
				s += signum(x[j] - x[i]) * signum(y[j] - y[i]);
		out[i] = s;
	}
	return out;
}

// O(n log n) Fenwick-tree concordance
[[cpp11::register]]
integers concordance_fenwick_cpp(doubles x, doubles y) {
	int n = x.size();
	if (n == 0) return writable::integers(static_cast<R_xlen_t>(0));
	if (n == 1) return writable::integers({0});

	// 1) order by x (stable), breaking ties by y
	std::vector<int> ord(n);
	std::iota(ord.begin(), ord.end(), 0);
	std::stable_sort(ord.begin(), ord.end(), [&](int a, int b) {
		return x[a] != x[b] ? x[a] < x[b] : y[a] < y[b];
	});

	// 2) rank-compress y to 1..m via indirect sort (no copies of x/y needed)
	std::vector<int> y_order(n);
	std::iota(y_order.begin(), y_order.end(), 0);
	std::sort(y_order.begin(), y_order.end(), [&](int a, int b) {
		return y[ord[a]] < y[ord[b]];
	});

	std::vector<int> ry(n);
	int m = 0;
	for (int i = 0; i < n; i++) {
		if (i == 0 || y[ord[y_order[i]]] != y[ord[y_order[i - 1]]])
			m++;
		ry[y_order[i]] = m;
	}

	// Fenwick tree (1-indexed) and frequency array to avoid a second BIT query
	std::vector<int> bit(m + 1, 0);
	std::vector<int> freq(m + 1, 0);
	std::vector<int> result(n, 0);

	auto update = [&](int i, int d) {
		for (; i <= m; i += i & (-i)) bit[i] += d;
	};
	auto query = [&](int i) {
		int s = 0;
		for (; i > 0; i -= i & (-i)) s += bit[i];
		return s;
	};

	// ---- Right sweep (groups right to left) ----
	int total = 0, g_end = n - 1;
	while (g_end >= 0) {
		int g_start = g_end;
		while (g_start > 0 && x[ord[g_start - 1]] == x[ord[g_end]])
			g_start--;

		for (int i = g_start; i <= g_end; i++) {
			int r    = ry[i];
			int less = query(r - 1);
			result[i] = (total - less - freq[r]) - less;
		}
		for (int i = g_start; i <= g_end; i++) {
			update(ry[i], 1);
			freq[ry[i]]++;
			total++;
		}
		g_end = g_start - 1;
	}

	// Reset
	std::memset(bit.data(),  0, (m + 1) * sizeof(int));
	std::memset(freq.data(), 0, (m + 1) * sizeof(int));
	total = 0;

	// ---- Left sweep (groups left to right) ----
	int g_start = 0;
	while (g_start < n) {
		int ge = g_start;
		while (ge < n - 1 && x[ord[ge + 1]] == x[ord[g_start]])
			ge++;

		for (int i = g_start; i <= ge; i++) {
			int r    = ry[i];
			int less = query(r - 1);
			result[i] += less - (total - less - freq[r]);
		}
		for (int i = g_start; i <= ge; i++) {
			update(ry[i], 1);
			freq[ry[i]]++;
			total++;
		}
		g_start = ge + 1;
	}

	// Restore original order
	writable::integers out(n);
	for (int i = 0; i < n; i++)
		out[ord[i]] = result[i];
	return out;
}
