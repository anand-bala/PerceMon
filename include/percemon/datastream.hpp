/// This file essentially defines what is representative of a stream of
/// perception data.
///
/// A perception data stream is a discrete signal consisting of frames, with
/// fixed sampling rate or frames per second. Each frame consists of frame
/// number/time stamp and a map associating IDs with labelled objects.
///
/// Each labelled object should contain the following:
///
/// - Class: Type of the object, outputted from some object detection algorithm.
/// - Probability: Probability associated with the class.
/// - A bounding box.

#pragma once

#ifndef __PERCEMON_STREAM_HH__
#define __PERCEMON_STREAM_HH__

#include <cstddef>
#include <map>
#include <string>

namespace percemon::datastream {

constexpr double DEFAULT_FPS = 30.0;
constexpr auto NS_PER_S      = 1000ULL * 1000ULL * 1000ULL;

/// Timestamp type compatible with ROS Time.
struct TimeStamp {
 private:
  std::uint64_t m_nanosec;

 public:
  TimeStamp(std::uint32_t seconds, std::uint32_t nanoseconds) {
    m_nanosec = seconds * NS_PER_S;
    m_nanosec += nanoseconds;
  }

  constexpr bool operator==(const TimeStamp& rhs) const {
    return m_nanosec == rhs.m_nanosec;
  }

  constexpr bool operator!=(const TimeStamp& rhs) const {
    return !(*this == rhs);
  }

  constexpr bool operator<(const TimeStamp& rhs) const {
    return m_nanosec < rhs.m_nanosec;
  }

  constexpr bool operator<=(const TimeStamp& rhs) const {
    return m_nanosec <= rhs.m_nanosec;
  }

  constexpr bool operator>=(const TimeStamp& rhs) const {
    return !(*this < rhs);
  }

  constexpr bool operator>(const TimeStamp& rhs) const {
    return !(*this <= rhs);
  }

  [[nodiscard]] constexpr double seconds() const {
    return static_cast<double>(m_nanosec) / static_cast<double>(NS_PER_S);
  }
};

/// A bounding box data structure that follows the Pascal VOC Bounding box format
/// (x-top left, y-top left,x-bottom right, y-bottom right), where each
/// coordinate is in terms of number of pixels.
///
/// @note The origin in an image is the top left corner.
struct BoundingBox {
  long long int xmin;
  long long int xmax;
  long long int ymin;
  long long int ymax;
};

/// An object in a frame has a Class, Probability, and BoundingBox associated with it.
struct Object {
  int class_id;
  std::string class_label;
  double probability;
  BoundingBox bbox;
};

struct Frame {
  /// @brief Time stamp
  TimeStamp timestamp;
  /// @brief Frame number
  size_t frame_num;

  /// @brief The size of the frame/image in pixels
  std::pair<size_t, size_t> size;

  /// @brief Map of objects keyed by their IDs
  std::map<std::string, Object> objects;
};

} // namespace percemon::datastream

#endif /* end of include guard: __PERCEMON_STREAM_HH__ */
