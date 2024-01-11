# nbully

An OTP application for leader election.

The nbully application starts a supervisor that supervises a worker process. This process runs in every node and consensuates node leadership with its homologues in other nodes.

## Status
[![GitHub branch checks state](https://github.com/nomasystems/nbully/actions/workflows/ci.yml/badge.svg)](https://github.com/nomasystems/nbully/actions/workflows/ci.yml)

## Prerequisites

![Min. OTP version](https://img.shields.io/badge/min._OTP-25.3.2-blue)
![Max. OTP version](https://img.shields.io/badge/max._OTP-26-blue)
![Min. rebar version](https://img.shields.io/badge/min._rebar-3.22.X-blue)

## Usage

In your `rebar.config` file, add the dependency:
```erl
{deps, [
    {nbully, {git, "git@github.com:nomasystems/nbully.git", {branch, "main"}}}
]}.
```

Then, once the application is started, you can consult `nbully:leader/0` to know which node is the current leader. This value is the same for all nodes connected through `net_kernel` and might only change when nodes go up or down.

You can also use `nbully:subscribe/0` to receive live updates when the leader changes in the form of a message `{nbully_leader_update, Node}`. For example:

```erl
ok = nbully:subscribe(),
InitialLeader = nbully:leader(),
% [...]
NewLeader =
  receive
    {nbully_leader_update, Node} -> Node
  after 1000 -> InitialLeader
end,
LeaderIsMe = node() =:= NewLeader,
% [...]
ok = nbully:unsubscribe().
```
## Support

Any doubt or suggestion? Please, check out [our issue tracker](https://github.com/nomasystems/nbully/issues).

## Contributing

Pull requests are welcome. Please read the [contributing guidelines](CONTRIBUTING.md) to know more about contribution.

