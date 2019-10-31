
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;


public class Test {


    /**
     *
     * @param color to get complementary colors from it
     * @return a optional stream of complementary colors
     * ex: for red -> Optional(Stream("rose", "orange", "pink"))
     */
    static Optional<Stream<String>> getComplementaryColors(String color) {
        HashMap<String, Stream<String>> colors = new HashMap<>();
        colors.put("red", Stream.of("rose", "orange", "pink"));
        colors.put("blue", Stream.of("violet", "azure", "bluish"));
        colors.put("green", Stream.of("teal", "chartreuse", "greenish"));

        return Optional.ofNullable(colors.get(color));
    }

    static Stream<String> complementaryColors(List<String> colors) {

        Stream<String> result = colors.stream().flatMap(color ->
            getComplementaryColors(color).orElse(Stream.of(color + " is not a primary color"))
        );

        return result;
    }

    public static void main(String []args) {
        List<String> list = new ArrayList<>();
        list.add("red");
        list.add("black");
        list.add("green");
        list.add("white");
        list.add("blue");
        Stream<String> result = complementaryColors(list);


        result.forEach(System.out::println);
    }

}
