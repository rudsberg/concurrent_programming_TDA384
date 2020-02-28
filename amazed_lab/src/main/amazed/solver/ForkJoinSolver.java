package amazed.solver;

import amazed.maze.Maze;

import java.util.*;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver
    extends SequentialSolver
{
    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze)
    {
        super(maze);
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        this(maze);
        this.forkAfter = forkAfter;
    }

    private ForkJoinSolver(Maze maze, int start, ConcurrentSkipListSet<Integer> visited, ConcurrentMap<Integer, Integer> predecessor) {
        this(maze);
        super.start = start;
        super.visited = visited;
        super.predecessor = predecessor;
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute()
    {
        return parallelSearch();
    }

    /**
     * - you will fork new threads, and each thread will continue the search in a different part of the maze in parallel to the others.
     * Fork när det finns mer än en granne *som är ej besökt*. Testa N forks för N grannar om det är mer än 1 granne (kanske N-1, för huvudtråd).
     *
     * - what state has to be shared among parallel threads (visited, a stack frontier, and a map predecessor)
     * visited     --> trådsäker
     * frontier    --> borde kunna vara lokal. Checka mot visited, så fort man går till en granne, lägg då till in visited.
     * predecessor --> trådsäker (flera trådar kan lägga till samtidigt)
     *
     * - use thread safe version of shared data structures
     *
     * - modify seq with paralell solution
     *
     * ------------
     *
     * ForkJoinPool ansvarar för alla workers, ForkJoinTasks, vars konkreta klass RecursiveTask har returvärde.
     * ForkJoinTasks compute kör arbetet.
     *
     */

    private List<Integer> parallelSearch()
    {
        // one player active on the maze at start
        int player = maze.newPlayer(start);
        // start with start node
        frontier.push(start);
        // as long as not all nodes have been processed
        while (!frontier.empty()) {
            // get the new node to process
            int current = frontier.pop();

            if (allNeighboursVisited(current)) {
                return null;
            }

            // if current node has a goal
            if (maze.hasGoal(current)) {
                // move player to goal
                maze.move(player, current);
                // search finished: reconstruct and return path
                return pathFromTo(start, current);
            }

            if (notVisitedNeighbours(current) <= 1) {
                // if current node has not been visited yet
                if (!visited.contains(current)) {
                    // move player to current node
                    maze.move(player, current);
                    // mark node as visited
                    visited.add(current);
                    // for every node nb adjacent to current
                    for (int nb: maze.neighbors(current)) {
                        // add nb to the nodes to be processed
                        if (!visited.contains(nb)) {
                            frontier.push(nb);
                            predecessor.put(nb, current);
                        }
                    }
                }
            } else {
                List<ForkJoinSolver> subtasks = new ArrayList<>();

                for (int neighbor : maze.neighbors(current)) {
                    subtasks.add(new ForkJoinSolver(maze, neighbor, visited, predecessor));
                }

                for (ForkJoinSolver task: subtasks) {
                    task.fork();
                }

                for(ForkJoinSolver subtask : subtasks) {
                    List<Integer> path = subtask.join();
                    if (path != null) {
                        return path;
                    }
                }

            }

        }
        // all nodes explored, no goal found
        return null;
    }

    private boolean allNeighboursVisited(int current) {
        for (int neighbor: maze.neighbors(current)) {
            if (!visited.contains(neighbor))
                return false;
        }
        return true;
    }

    private int notVisitedNeighbours(int current) {
        int count = 0;
        for (int neighbor: maze.neighbors(current)) {
            if (!visited.contains(neighbor))
                count++;
        }
        return count;
    }

    protected List<Integer> pathFromTo(int from, int to) {
        List<Integer> path = new LinkedList<>();
        Integer current = to;
        while (current != from) {
            path.add(current);
            current = predecessor.get(current);
            if (current == null)
                return null;
        }
        path.add(from);
        Collections.reverse(path);
        return path;
    }
}
