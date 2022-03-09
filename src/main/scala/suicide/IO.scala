package ktedon.suicide

import scala.util.control.NonFatal

// Defines all IO methods.
object IO:

	// Takes a resource, a function on that resource, returns the
	// result of that function alongside the resource, and
	// safely closes the resource.
	def withResourcesAutoclose[T <: AutoCloseable, V](
		resource: T,
	)( f: T => V ): V =
		var exception: Throwable | Null = null

		try
			f(resource)
		catch
			case NonFatal(e) =>
				exception = e
				throw e
		finally
			closeAndAddSuppressed(exception, resource)

	// Safely closes a resource with an exception.
	def closeAndAddSuppressed(
		e: Throwable | Null,
		resource: AutoCloseable
	): Unit =

		if (e != null)
			try
				resource.close()
			catch
				case NonFatal(suppressed) =>
					e.nn.addSuppressed(suppressed)
		else
			resource.close()
