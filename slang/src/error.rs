#[allow(clippy::needless_range_loop)]
pub fn source_error(input: &str, start: usize, end: usize, message: impl ToString) -> String {
    let chars = input.chars().collect::<Vec<_>>();

    let mut line_start_index = 0;
    let mut column_start_index = 0;
    // let mut line_start_num = 0;
    for index in 0..start {
        if chars[index] == '\n' {
            line_start_index = index + 1;
            column_start_index = 0;
            // line_start_num += 1;
        } else {
            column_start_index += 1;
        }
    }

    let mut line_end_index = chars.len();
    // let mut column_end_index = column_start_index;
    // let mut line_end_num = line_start_num;
    for index in start..end {
        if chars[index] == '\n' {
            line_end_index = index + 1;
            // column_end_index = 0;
            // line_end_num += 1;
        } else {
            // column_end_index += 1;
        }
    }

    // TODO: Handle multiline errors

    let mut error = String::new();
    error.push_str(&format!(
        "{} line {} column {}\n",
        message.to_string(),
        line_start_index + 1,
        column_start_index + 1,
    ));
    error.push_str(&format!(
        "{}\n",
        chars[line_start_index..line_end_index]
            .iter()
            .collect::<String>(),
    ));
    error.push_str(&format!("{:column$}^", "", column = column_start_index));
    error
}
