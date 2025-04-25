package iprotogen

import "testing"

func TestGeneratedFileName(t *testing.T) {
	tests := []struct {
		in, want string
	}{
		{"model.go", "model_generated.go"},
		{"model_test.go", "model_generated_test.go"},
		{"no_ext", "no_ext_generated"},
		{"no_ext_test", "no_ext_generated_test"},
	}

	for _, tt := range tests {
		got := GeneratedFileName(tt.in)

		if got != tt.want {
			t.Errorf("GeneratedFileName(%q): got %q, want %q", tt.in, got, tt.want)
		}
	}
}
