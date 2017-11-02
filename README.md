# CH/OTP Test Task
by Kamil Figiela <kamil.figiela@gmail.com>

While the goal of the task is well defined, it still has a lot of moving parts. Therefore I did a number of assumptions that I will cover in this document.

# Quick Start Guide

1. `stack setup`
2. `stack build`
3. `./run.sh`

The project can be built with stack. The node binary may be run with stack `stack exec -- dptt`. The example startup script is provided (`run.sh`) for starting a few nodes locally.

# Options

It is obvious requirement to run nodes on different endpoints, so they can bind ports. It's strongly suggested to configure node discovery by providing list of known peers with `--peers-file` option. The file with peer list should be in `host:port` format.

```
$ stack exec -- dptt --help
Help Options:
  -h, --help
    Show option summary.
  --help-all
    Show all help options.

Application Options:
  --send-for :: int
    How many seconds the system sends the messages.
    default: 10
  --wait-for :: int
    How many seconds the system waits for undelivered messages.
    default: 10
  --with-seed :: int
    Random number generator seed.
    default: 1
  --connect-for :: int
    Time to discover other peers.
    default: 0
  --host :: text
    Hostname for local node.
    default: "127.0.0.1"
  --port :: int
    Port for local node.
    default: 10000
  --peers-file :: maybe<text>
    File with known peers list (in host:port per line format).
  --multicast-discovery :: bool
    Whether to use multicast discovery in addition to peer exchange.
    default: false
  --buffer-mult :: int
    Buffer length multiplier (1 = buffer up to 10 000 messages).
    default: 4
  --send-delay :: int
    Delay between messages in microseconds. Use this to control throughput.
    default: 10000
```

# Network Latency Emulation

On Mac network latency emulation for test setup (`run.sh`) may be set up with the `setup_latency_mac.sh` script and disabled with `disable_latency_mac.sh`. The scripts configure certain link conditions on particular TCP ports.

# Example execution log

Recorded executon under network with latency condition is available at asciinema: https://asciinema.org/a/145610

# Assumptions & Design
## Timestamps & Clock Synchronization

The clock synchronization problem in distributed systems is non-trivial and is often aided by use of logical clocks (Lamport clock, vector clock), but they still provide a partial ordering. In real world, function τ does not exist.

For the sake of simplicity I decided to rely on synchronization of wall-time clocks on the machines running nodes. I assume that clients will have NTP daemon running that will provide time synchronization in order of tens of miliseconds. However, we need to be also aware of not ideal clock accuracy (some clocks running slightly faster than others), so there will be some clock drift. Higher clock accuracy, if required, is often achieved by using GPS signal as precise clock source (apart of atomic clocks etc.).

## Message Ordering

Using wall-time clock still has some gotchas – multiple messages may be sent with the same timestamp. Either, by multiple nodes, but also by single node if clock resolution is low (in my setup clock resolution reported by the system is 1 microsecond, not so bad).

If only timestamp was used for ordering messages, different scores may be computed for the same set of messages, depending on e.g. message arrival order. In order to improve consensus between nodes, extra assumption needs to be made.

The messages contain random number, timestamp, sequence number and sending node id. I make an extra assumption to order messages not only by timestamp, but also by sending node id and sequence number being sent from particular node. 

This guarantees single ordering for any set of messages. On the other hand, nodes with “lower” node id will have some preference over the others. This is not necessarily a good thing we want to have. Alternatively, we could sort messages by their value (random number) 

## Communication

In my implementation all nodes are equal, there is no single master node and nodes are not aware of all other nodes at start (or even of total size of the cluster). All nodes are started independently. I decided to use peer-to-peer communication model (without using `distributed-process-p2p` library which seems to be broken right now).

I assume that all nodes are reachable in terms of network connectivity (firewalls, NATs, broken routing). This assumption could be loosened by implementing some sort of routing algorithm if no direct communication is possible. In some non-trivial version that could use the similar principles as routing protocols used in networks (like RIP, OSPF or BGP).

## Network Congestion

One of the options worth noting is `--send-delay`. This option allows to control bandwidth used by all nodes (larger delay results in fewer messages per second). This helps to get meaningful results if some nodes have very slow link that gets easily saturated.

## Node Discovery

Point 2.3 of the task description suggests that nodes do not necessarily have upfront knowledge of other nodes in the network. To aid this issue, I implement simple (really) peer exchange protocol.

Each node may be provided with initial list of known nodes. To do this one needs to create a file in format `host:port` per line and it’s location is provided by `--peers-file knownPeers.txt`.

After startup, nodes try to connect with all nodes supplied. They do this by sending their known peers list to the other node. Each node, upon receiving known nodes from the other node merges it to local known peers list. If it was modified by doing that, it sends this merged version to all known peers. Ultimately, all nodes should have the same peer list. While this protocol is suboptimal as it involves more communication than necessary, it still does its job.

Additionally, known peers list is retransmitted to all known peers that have not yet responded a few times per second, to ensure a quick cluster warmup. 

Nodes start to send data just after startup. For testing purposes, I allow for a certain warmup period for peer exchange (configured with `--connect-for` option, defaults to 0). After this period data transmission starts. However, this is not required for data consistency, since I implement retransmissions (as long as data to be retransmitted is in local buffer).

## Retransmissions

Some initial messages may be lost, since nodes start to send messages before they connect to all other nodes. To aid this problem, each message contains sequence number. Data collectors keep track of last message number received from each node. If message loss is detected, receiving node asks the sending node to retransmit certain range of messages. Those messages are retreived from data collector buffer of sending node if they are still in “hot” buffer (haven’t been taken to compute partial result yet).

## Memory Usage

In order to prevent memory leaks I use strict version of fold and strict constructor for collector state. This protects from accumulation of non-evaluated thunks.

Another concern is receiver message buffer. Receiver maintains a buffer of incoming messages represented as a set. To make it possible to have really long runs of the program, I implement special algorithm to precompute result on the fly.

For each known node, I compute maximum timestamp of continuous messages (means no message between was lost). Then I compute minimum of those (t_min). Since for all other nodes I have messages with newer timestamp, no messages from them will happen before t_min and I can safely compute partial result for all messages before it.
Precomputing of results happen when the buffer size exceeds threshold value and is only performed if we got messages from all known peers.
The drawback of this solution is that after precomputing result it is impossible to retransmit messages which are no longer in message buffer.

This safe algorithm does not help if some nodes die and the buffer starts growing, since t_min will not be raising. In such case, there is another threshold and if it’s exceeded, I force precomputation of partial result for half that threshold. If missing messages are delivered after result is precomputed they are discarded. As we don't know number of peers in network upfront, this may happen when one of nodes connects late.

## Random Numbers

All nodes can use different DRNG seed and it can be configured with `--with-seed` option. 

## Trailing Message

Nodes send extra message upon finishing sending messages. When receiver gets such message from all nodes it can compute and display final result. Otherwise, the result will be computed just before final deadline.

# Conclusions

This not so complex distributed algorithm shows perfectly how many things have to be concerned when designing distributed system. The problem cannot be solved in a one way and assumptions, tradeoffs and arbitrary decisions have to be made. I did certain choices focusing on: data consistency (retransmissions), contained memory consumption (periodical precomputation), lack of central server (all nodes are equal).

I did testing on my local machine with simulated network conditions. The algorithm is provides consistent results most of the time, however there are some conditions that are likely to break things and should be subject for more detailed algorithm fine-tuning:

* network latency combined with packet loss – TCP connection gets really slow,
    * it takes ages for node discovery – data loss is likely to happen since other nodes may precompute results without being aware of particular node even existing, this can be aided partially by setting `--connect-for`,
    * final messages arrive after deadline, aided by `--wait-for`,
* nodes send messages too fast – especially on slow networks with some latency, data loss happens – throughput can be controlled with `--send-delay`,


